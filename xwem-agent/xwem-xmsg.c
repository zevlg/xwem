/*
 * xwem-xmsg.c --- Simple utility to send Xmessages.
 *
 ** Author: Zajcev Evgeny <zevlg@yandex.ru>
 ** Created: Sat Sep 13 17:38:54 MSK 2008
 ** Keywords: xwem, X11
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
 * $Id: init-home.el,v 1.8 2008/07/25 00:02:17 lg Exp $
 */
#include <X11/X.h>
#include <X11/Xlib.h>

#include <sysexits.h>
#include <stdlib.h>
#include <stdio.h>

void
usage(const char *progn)
{
        printf("usage: %s <atom-name> [window-id]\n", progn);
        printf("\twindow-id\tWindow it to send message (default: root window)\n");
        exit(EX_USAGE);
}

void
XClientMessage(Display* xdpy, Atom msg, Window w)
{
        XClientMessageEvent cev;

        cev.type = ClientMessage;
        cev.window = w;
        cev.message_type = msg;

        (void) XSendEvent(xdpy, w, False, NoEventMask, (XEvent*)&cev);
}

int
main(int ac, char **av)
{
        Display* xdpy;
        Atom atom;
        Window rwin;

        if (ac < 2)
                usage(av[0]);

        xdpy = XOpenDisplay(NULL);
        if (xdpy == NULL) {
                fprintf(stderr, "- Can't open display\n");
                exit(EX_NOINPUT);
        }

        atom = XInternAtom(xdpy, av[1], False);
        if (atom == None) {
                fprintf(stderr, "- Atom %s not found\n", av[1]);
                exit(EX_UNAVAILABLE);
        }

        rwin = RootWindow(xdpy, DefaultScreen(xdpy));
        XClientMessage(xdpy, atom, rwin);
        printf("+ Message %s sent\n", av[1]);
        return 0;
}
