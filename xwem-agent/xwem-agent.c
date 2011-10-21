/*
 * xwem-agent.c --- Simple session agent for XWEM.
 *
 * Copyright (C) 2005 XWEM Org.
 *
 ** Author: Zajcev Evgeny <zevlg@yandex.ru>
 ** Created: Sun Feb 13 01:38:54 MSK 2005
 ** Keywords: xwem, agent, X11
 *
 * This file is part of XWEM.
 *
 * XWEM is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * XWEM is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
 * License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with XEmacs; see the file COPYING.  If not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *
 ** Synched up with: Not in FSF
 *
 ** Comentary:
 *
 * This is an X11 agent to be used with xwem.  xwem is pretty stable
 * this time (2.1-patch-20), however (S)XEmacs is not stable enought.
 * So if any problem occurs and somebody(including xwem) causes
 * problem to XEmacs and hang it or crash, we lost X session and all
 * running clients.  It is much of pain.  The idea to write xwem-agent
 * firstly occured on #xwem channel by a discussion between myself and
 * Steve Youngs about 6 to 12 months ago.  The idea was recently
 * expanded upon an improved after a discussion on IRC with
 * alex-i(Please alex-i name your self, to include your name here) in
 * very interesting discussion about how window manager and desktop
 * environment should be built.  Many thanks to alex-i for an excelent
 * idea.
 *
 * When xwem-agent starts it install magic keys (C-Sh-F6 and C-Sh-F11)
 * by default and starts (S)XEmacs.  Whenever you want to restart
 * (S)XEmacs - press one of the magic keys.  You can also force
 * (S)XEmacs restart by killing xwem-agent with SIGHUP signal.
 *
 *   C-Sh-F6  - Kill (S)XEmacs with SIGABORT signal, this will cause
 *              (S)XEmacs to dump core for futher investigation.
 *   C-Sh-F9  - Kill (S)XEmacs with SIGKILL signal, this will cause
 *              (S)XEmacs to always exit.
 *   C-Sh-F11 - Kill (S)XEmacs with SIGTERM signal, this will cause
 *              (S)XEmacs to terminate normally.
 *   C-Sh-ESC - Exit xwem-agent.
 *
 ** Usage:
 *
 * To start using xwem-agent, simple copy xwem-agent somewhere in your
 * execution path and add to your ~/.xinitrc this:
 *
 *     # exec xemacs
 *     exec xwem-agent -f ~/.xwem/xwem-agent.xemacs.log xemacs
 * 
 * Or if using sxemacs:
 *
 *     # exec sxemacs
 *     exec xwem-agent -f ~/.xwem/xwem-agent.sxemacs.log sxemacs
 *
 * Restart your X and be happy.
 *
 ** TODO:
 *
 * - Add item1 here
 * - Add item2 here
 *
 */
#include <X11/X.h>
#include <X11/Xlib.h>
#define XK_MISCELLANY
#include <X11/keysymdef.h>

#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>

#include <time.h>
#include <signal.h>
#include <fcntl.h>

#include <stdarg.h>

#include <sysexits.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>

Display *xdpy;
pid_t epid = -1;                        /* emacs pid */

enum {
        VERBOSE_NONE,
        VERBOSE_STDOUT,
        VERBOSE_X,
        VERBOSE_BOTH
};
int verbose = VERBOSE_BOTH;
int autodetect = 1;

char *emacs;
char **emacs_argv;

char *outfile = NULL;

enum {
        STATE_WAITING,
        STATE_RUNNING,
        STATE_KILLING
};
static int state = STATE_WAITING;      /* xwem-agent current state */

/*
 * Evil hackery do display verbose logs
 */
int
xverbose(const char *fmt, ...)
{
#define VSTRLEN 256
        static char vstr[VSTRLEN+1];
        int ret;
        va_list ap;

        va_start(ap, fmt);
        ret = vsnprintf(vstr, VSTRLEN, fmt, ap);
        va_end(ap);

        if (verbose == VERBOSE_STDOUT || verbose == VERBOSE_BOTH)
                printf("%s\n", vstr);

        if (verbose == VERBOSE_X || verbose == VERBOSE_BOTH) {
                static int y = 13;
                XDrawImageString(xdpy, RootWindow(xdpy, DefaultScreen(xdpy)),
                                 DefaultGC(xdpy, DefaultScreen(xdpy)),
                                 0, y, vstr, strlen(vstr));
                y += 13;
                if (y > 600) {
                        XClearWindow(xdpy, RootWindow(xdpy, DefaultScreen(xdpy)));
                        y = 13;
                }
                XFlush(xdpy);
        }

        return ret;
}

pid_t
start_emacs()
{
        if (epid > 0) {
                /* Already running emacs */
                xverbose("  - (S)XEmacs already running, not starting");
                return -1;
        }

        if ((epid = fork()) == 0) {
                setenv("XWEM_RUNNING", "notyet", 1);

                /* Direct (S)XEmacs stdout/stderr to file */
                if (outfile) {
                        int fd = open(outfile, O_CREAT|O_WRONLY|O_APPEND,
                                      S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH);
                        if (fd > 0) {
                                time_t clock = time(NULL);
                                char *ts = ctime(&clock);

                                write(fd, "------------[ ", 14);
                                write(fd, ts, strlen(ts));

                                close(STDOUT_FILENO);
                                close(STDERR_FILENO);
                                dup2(fd, STDOUT_FILENO);
                                dup2(fd, STDERR_FILENO);
                        }
                }

                /* Remove signal handlers */
                signal(SIGCHLD, SIG_DFL);
                signal(SIGHUP, SIG_DFL);

                execvp(emacs, emacs_argv);

                /* Execve failed :( */
                fprintf(stderr, "    - execve failed: %s\n", strerror(errno));
                exit(EX_UNAVAILABLE);
                /* NOT REACHED */
        }
        state = STATE_RUNNING;
        xverbose("  + Starting %s pid=%d ..", emacs, epid);

        return epid;
}

void
restart_emacs(int sig)
{
        xverbose("+ Restarting (S)XEmacs ..");

        /* Kill (S)XEmacs */
        if (epid > 0) {
                xverbose("  + Killing (S)XEmacs pid=%d sig=%d ..", epid, sig);

                state = STATE_KILLING;
                kill(epid, sig);
        } else
                start_emacs();
}

void
emacs_rip(int sig)
{
        pid_t pid;

        xverbose("+ SIGCHLD received ..");

        while ((pid = waitpid(epid, NULL, WNOHANG)) > 0) {
                xverbose("  + Riping (S)XEmacs pid=%d ..", pid);
                if (pid == epid)
                        epid = -1;
        }

        XSetInputFocus(xdpy, PointerRoot, RevertToPointerRoot, CurrentTime);
        xverbose("  + InputFocus set to PointerRoot ..");

        if ((state == STATE_KILLING) || autodetect) {
                if (start_emacs() < 0)
                        state = STATE_WAITING;
        } else
                state = STATE_WAITING;
}

void
emacs_hup(int sig)
{
        xverbose("+ SIGHUP received ..");
        restart_emacs(SIGTERM);
}

void
usage(const char *prog)
{
        printf("usage: %s [-on] [-f <outfile>] [-v X|stdout|both|none] "
               "<xemacs|sxemacs> [emacs arguments]\n"
               "\t-o\tOmit starting (S)XEmacs on start.\n"
               "\t-n\tDo not autostart new (S)XEmacs when old exits.\n"
               "\t-f\tSpecify output file."
               " (S)XEmacs will direct its output to this file.\n"
               "\t-v\tEnable/disable verbosity type. Default is 'both'\n",
               prog);
        exit(EX_USAGE);
}

int
main(int argc, char **argv)
{
        const char *prog = argv[0];
        int o_flag = 0;
        int ch;
        KeyCode exit_kc, abort_kc, term_kc, kill_kc;

        while ((ch = getopt(argc, argv, "onv:f:")) != -1) {
                switch (ch) {
                case 'o':
                        o_flag = 1;
                        break;
                case 'v':
                        if (!strcmp(optarg, "X"))
                                verbose = VERBOSE_X;
                        else if (!strcmp(optarg, "stdout"))
                                verbose = VERBOSE_STDOUT;
                        else if (!strcmp(optarg, "both"))
                                verbose = VERBOSE_BOTH;
                        else if (!strcmp(optarg, "none"))
                                verbose = VERBOSE_NONE;
                        else
                                usage(prog);
                        break;
                case 'n':
                        autodetect = 0;
                        break;
                case 'f':
                        outfile = optarg;
                        break;
                case '?':
                default:
                        usage(prog);
                }
        }
        argc -= optind;
        argv += optind;
        
        if (argc < 1)
                usage(prog);

        /* Setup emacs arguments */
        emacs = argv[0];
        emacs_argv = argv;

        xdpy = XOpenDisplay(NULL);
        if (xdpy == NULL) {
                fprintf(stderr, "- Can't open display\n");
                exit(EX_NOINPUT);
        }

        /* Install signal handler to rip childs */
        signal(SIGCHLD, emacs_rip);
        signal(SIGHUP, emacs_hup);

        xverbose("+ X Initialisation ..");

        XSetInputFocus(xdpy, RootWindow(xdpy, DefaultScreen(xdpy)),
                       RevertToPointerRoot, CurrentTime);
        xverbose("  + InputFocus set to root window ..");

        /* Grab magic key */
        exit_kc = XKeysymToKeycode(xdpy, XK_Escape);
        XGrabKey(xdpy, exit_kc, ShiftMask|ControlMask,
                 RootWindow(xdpy, DefaultScreen(xdpy)), True,
                 GrabModeAsync, GrabModeAsync);
        abort_kc = XKeysymToKeycode(xdpy, XK_F6);
        XGrabKey(xdpy, abort_kc, ShiftMask|ControlMask,
                 RootWindow(xdpy, DefaultScreen(xdpy)), True,
                 GrabModeAsync, GrabModeAsync);
        term_kc = XKeysymToKeycode(xdpy, XK_F11);
        XGrabKey(xdpy, term_kc, ShiftMask|ControlMask,
                 RootWindow(xdpy, DefaultScreen(xdpy)), True,
                 GrabModeAsync, GrabModeAsync);
        kill_kc = XKeysymToKeycode(xdpy, XK_F9);
        XGrabKey(xdpy, kill_kc, ShiftMask|ControlMask,
                 RootWindow(xdpy, DefaultScreen(xdpy)), True,
                 GrabModeAsync, GrabModeAsync);
        XFlush(xdpy);
        xverbose("  + Magic keys at C-Sh-ESC(exit), C-Sh-F6(SIGABORT),"
                 " C-Sh-F9(SIGKILL) and C-Sh-F11(SIGTERM) ..");

        state = STATE_WAITING;

        if (o_flag == 0)
                /* Start (S)XEmacs */
                start_emacs();

        /* Event loop */
        while (1) {
                XEvent xev;

                XNextEvent(xdpy, &xev);
                switch (xev.type) {
                case KeyPress:
                        xverbose("+ Magic KeyPress (S)XEmacs pid=%d ..", epid);

			if (xev.xkey.keycode == exit_kc) {
				xverbose("  + Exiting ..");
				XCloseDisplay(xdpy);
				exit(EX_OK);
                        } else if (xev.xkey.keycode == abort_kc)
                                restart_emacs(SIGABRT);
                        else if (xev.xkey.keycode == term_kc)
                                restart_emacs(SIGTERM);
                        else if (xev.xkey.keycode == kill_kc)
                                restart_emacs(SIGKILL);
                        else
                                xverbose("  - Unrecognized keycode 0x%X ..",
                                         xev.xkey.keycode);
                        break;
                }
        }

        return 0;
}
