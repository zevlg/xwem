## Makefile for XWEM   -*-Makefile-*-

##
## Copyright (C) 2004 Steve Youngs
##
## This file is part of XWEM.

## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions
## are met:
##
## 1. Redistributions of source code must retain the above copyright
##    notice, this list of conditions and the following disclaimer.
##
## 2. Redistributions in binary form must reproduce the above copyright
##    notice, this list of conditions and the following disclaimer in the
##    documentation and/or other materials provided with the distribution.
##
## 3. Neither the name of the author nor the names of any contributors
##    may be used to endorse or promote products derived from this
##    software without specific prior written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
## IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
## WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
## DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
## FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
## CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
## SUBSTITUTE GOODS OR SERVICES# LOSS OF USE, DATA, OR PROFITS# OR
## BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
## WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
## OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
## IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

### You MUST Edit This Variable!!!
# This is the _absolute_ path to your XLIB lisp directory.
XLIB_PATH = 

export PACKAGE = xwem
export VER = 2.2

# csh... yell no, we won't go!
SHELL = /bin/sh

# Programs and their flags.
ifndef XEMACS
XEMACS = sxemacs
endif
XEMACS_FLAGS = -batch -no-autoloads
INSTALL = install
PKG_INSTALL = install
TAR = tar
TAR_FLAGS = czf

# Our prefix.  Everything hangs off this.  
ifndef PREFIX
  ifeq ('$(XEMACS)','sxemacs')
    PREFIX = /usr/local/share/sxemacs/site-packages
  else
    PREFIX = /usr/local/lib/xemacs/site-packages
  endif
endif
prefix = $(PREFIX)

# Where the lisp files go.
LISP_DIR = $(prefix)/lisp/$(PACKAGE)

# Where the info files go.
INFO_DIR = $(prefix)/info

# Where the texinfo sources go.
DOC_DIR = $(prefix)/man/$(PACKAGE)

# Where the pixmaps go.
DATA_DIR = $(prefix)/etc/$(PACKAGE)

# If you want to make a tarball that you can just unpack on all your
# PC's you can 'make pkg'.  The 'pkg' target uses these directories to
# build the tarball.
STAGING = ../build-pkg
INFO_STAGING = $(STAGING)/info
LISP_STAGING = $(STAGING)/lisp/$(PACKAGE)
DATA_STAGING = $(STAGING)/etc/$(PACKAGE)
DOC_STAGING = $(STAGING)/man/$(PACKAGE)

############################################################################
##                No User Configurable Items Below Here                   ##
############################################################################

# Automatically generated sources
AUTO_SOURCES=./lisp/xwem-loaddefs.el ./lisp/auto-autoloads.el \
             ./lisp/custom-load.el ./lisp/xwem-version.el

# Static sources
SOURCES = $(filter-out $(AUTO_SOURCES), \
		$(wildcard ./dockapp/*.el) \
		$(wildcard ./extra/*.el) \
		$(wildcard ./lisp/*.el) \
		$(wildcard ./utils/*.el))

OBJECTS = $(SOURCES:.el=.elc)

EXTRA_SRC = $(AUTO_SOURCES)

# Can't use $(wildcard *) here because these files probably won't
# exist when this variable gets read in.
EXTRA_OBJ = ./lisp/auto-autoloads.elc ./lisp/xwem-loaddefs.elc \
	    ./lisp/xwem-version.elc ./lisp/custom-load.elc

DATA_FILES = $(wildcard ./icons/*.xpm) \
	./icons/README \
	./logo.xpm

INFO_FILES = ./man/$(PACKAGE).info
TEXI_FILES = $(wildcard ./man/*.texi)

SETUP_PATHS = -eval \
	"(progn \
	  (push \"$(XLIB_PATH)\" load-path) \
          (push \"./dockapp\" load-path) \
          (push \"./extra\" load-path) \
          (push \"./lisp\" load-path) \
          (push \"./utils\" load-path) \
	  (push default-directory load-path))"

PRELOADS = $(SETUP_PATHS) -l lpath.el -l ./lisp/auto-autoloads.el

AUTOLOAD_PACKAGE_NAME = (setq autoload-package-name \"$(PACKAGE)\")
AUTOLOAD_FILE = (setq generated-autoload-file \"./lisp/auto-autoloads.el\")


.SUFFIXES:
.SUFFIXES: .elc .el

ifeq ($(XLIB_PATH),)
all:
	@echo "You haven't specified a path to your XLIB lisp directory"
	@echo "Either edit the Makefile and change XLIB_PATH ="
	@echo "Or run make like 'make XLIB_PATH=/path/to/xlib/lisp'"
else
all:: compile customloads texinfo
endif

loaddefs: ./lisp/xwem-loaddefs.el
version: ./lisp/xwem-version.el
customloads: ./lisp/custom-load.el
autoloads: version ./lisp/auto-autoloads.el

compile: autoloads loaddefs $(EXTRA_OBJ) $(OBJECTS)

%.elc: %.el
	$(XEMACS) $(XEMACS_FLAGS) $(PRELOADS) -l bytecomp \
		-f batch-byte-compile $<

./lisp/xwem-loaddefs.el:  $(SOURCES)
	$(XEMACS) $(XEMACS_FLAGS) $(SETUP_PATHS) \
		-l xwem-loaddefs-gen \
		-f xwem-batch-update-directory

./lisp/auto-autoloads.el: $(SOURCES)
	$(XEMACS) $(XEMACS_FLAGS) \
		-eval "$(AUTOLOAD_PACKAGE_NAME)" \
		-eval "$(AUTOLOAD_FILE)" \
		-l cl-macs -l autoload -f batch-update-autoloads $^

./lisp/custom-load.el: $(SOURCES)
	$(XEMACS) $(XEMACS_FLAGS) $(SETUP_PATHS) -l xwem-custom-gen \
		-f xwem-batch-custom-make-deps lisp dockapp extra lisp utils

./lisp/xwem-version.el: .
	echo ";;; Automatically generated file -- DO NOT EDIT OR DELETE" > $@
	echo ";;;###autoload" >> $@
	echo "(defconst xwem-version" >> $@
	if [ -d "./.git" -a -x `type git 2>/dev/null` ]; then \
		printf '  "%s"' `git describe | tail -n1` >> $@; \
	else \
		echo -n '  "$(VER)"' >> $@; \
	fi
	echo ")" >> $@
	echo "(provide 'xwem-version)" >> $@

install: $(SOURCES) $(EXTRA_SRC) $(OBJECTS) $(EXTRA_OBJ) $(DATA_FILES) texinfo
	$(INSTALL) -d $(INFO_DIR) $(LISP_DIR) $(DATA_DIR) $(DOC_DIR)
	$(INSTALL) -m 644 $(INFO_FILES) $(INFO_DIR)
	$(INSTALL) -m 644 $(TEXI_FILES) $(DOC_DIR)
	$(INSTALL) -m 644 $(DATA_FILES) $(DATA_DIR)
	$(INSTALL) -m 644 $(SOURCES) $(EXTRA_SRC) $(OBJECTS) $(EXTRA_OBJ) \
		$(LISP_DIR)

pkg: $(SOURCES) $(EXTRA_SRC) $(OBJECTS) $(EXTRA_OBJ) $(DATA_FILES) texinfo
	$(PKG_INSTALL) -d $(STAGING) $(INFO_STAGING) $(LISP_STAGING) \
		$(DATA_STAGING) $(DOC_STAGING)
	$(PKG_INSTALL) -m 644 $(INFO_FILES) $(INFO_STAGING)
	$(PKG_INSTALL) -m 644 $(TEXI_FILES) $(DOC_STAGING)
	$(PKG_INSTALL) -m 644 $(DATA_FILES) $(DATA_STAGING)
	$(PKG_INSTALL) -m 644 $(SOURCES) $(EXTRA_SRC) $(OBJECTS) $(EXTRA_OBJ) \
		$(LISP_STAGING)
	(cd $(STAGING); \
		rm -f $(PACKAGE)-$(VER)-pkg.tar.gz; \
		$(TAR) $(TAR_FLAGS) $(PACKAGE)-$(VER)-pkg.tar.gz \
			./etc/$(PACKAGE) ./lisp/$(PACKAGE) \
			./info/$(PACKAGE).info ./man/$(PACKAGE) )

upgrade: uninstall install

uninstall:: 
	rm -rf $(LISP_DIR) $(DATA_DIR) $(DOC_DIR)
	rm -f $(INFO_DIR)/$(INFO_FILES)

clean::
	rm -f $(AUTO_SOURCES) $(OBJECTS) $(EXTRA_OBJ)
	cd man && $(MAKE) clean
	cd xwem-agent && $(MAKE) clean

distclean: clean
	rm -f core* *~ *.TAGS ./lisp/*~ ./dockapp/*~ ./extra/*~ ./utils/*~ \
		./lisp/*.elc ./dockapp/*.elc ./extra/*.elc ./utils/*.elc

texinfo:
	cd man && $(MAKE)

# Developer targets
tags: TAGS

TAGS: $(SOURCES)
	etags $(SOURCES)


#### DEPS
lisp/xwem-load.elc: lisp/xwem-loaddefs.el lisp/xwem-struct.el lisp/xwem-interactive.el lisp/xwem-faces.el
lisp/xwem-win.elc: lisp/xwem-load.el
lisp/xwem-frame.elc: lisp/xwem-load.el lisp/xwem-misc.el
lisp/xwem-clgen.elc: lisp/xwem-load.el lisp/xwem-manage.el lisp/xwem-misc.el
lisp/xwem-clients.elc: lisp/xwem-load.el lisp/xwem-manage.el lisp/xwem-misc.el
lisp/xwem-clswi.elc: lisp/xwem-clients.el
lisp/xwem-desktop.elc: lisp/xwem-load.el lisp/xwem-frame.el
lisp/xwem-edmacro.elc: lisp/xwem-load.el
lisp/xwem-events.elc: lisp/xwem-load.el
lisp/xwem-faces.elc: lisp/xwem-struct.el lisp/xwem-loaddefs.el
lisp/xwem-focus.elc: lisp/xwem-load.el
lisp/xwem-help.elc: lisp/xwem-load.el lisp/xwem-misc.el
lisp/xwem-icons.elc: lisp/xwem-load.el
lisp/xwem-interactive.elc: lisp/xwem-struct.el lisp/xwem-loaddefs.el
lisp/xwem-keyboard.elc: lisp/xwem-load.el lisp/xwem-misc.el
lisp/xwem-keydefs.elc: lisp/xwem-load.el lisp/xwem-misc.el lisp/xwem-compat.el
lisp/xwem-keymacro.elc: lisp/xwem-load.el lisp/xwem-misc.el
lisp/xwem-launcher.elc: lisp/xwem-load.el
lisp/xwem-main.elc: lisp/xwem-load.el lisp/xwem-minibuffer.el
lisp/xwem-manage.elc: lisp/xwem-load.el
lisp/xwem-minibuffer.elc: lisp/xwem-load.el lisp/xwem-focus.el lisp/xwem-manage.el
lisp/xwem-misc.elc: lisp/xwem-load.el
lisp/xwem-modes.elc: lisp/xwem-load.el
lisp/xwem-mouse.elc: lisp/xwem-load.el lisp/xwem-manage.el
lisp/xwem-netwm.elc: lisp/xwem-load.el lisp/xwem-manage.el
lisp/xwem-register.elc: lisp/xwem-load.el lisp/xwem-help.el
lisp/xwem-root.elc: lisp/xwem-load.el lisp/xwem-misc.el
lisp/xwem-rooter.elc: lisp/xwem-load.el lisp/xwem-manage.el lisp/xwem-focus.el
lisp/xwem-selections.elc: lisp/xwem-load.el lisp/xwem-help.el
lisp/xwem-sound.elc: lisp/xwem-load.el
lisp/xwem-special.elc: lisp/xwem-load.el lisp/xwem-manage.el
lisp/xwem-strokes.elc: lisp/xwem-load.el lisp/xwem-misc.el
lisp/xwem-tabbing.elc: lisp/xwem-load.el
lisp/xwem-theme.elc: lisp/xwem-load.el
lisp/xwem-transient.elc: lisp/xwem-load.el lisp/xwem-focus.el lisp/xwem-manage.el
lisp/xwem-tray.elc: lisp/xwem-load.el lisp/xwem-help.el lisp/xwem-manage.el

utils/xwem-appcollect.elc: lisp/xwem-load.el lisp/xwem-manage.el lisp/xwem-launcher.el
utils/xwem-diagram.elc: lisp/xwem-faces.el
utils/xwem-holer.elc: lisp/xwem-load.el lisp/xwem-compat.el lisp/xwem-frame.el
utils/xwem-osd.elc: utils/xwem-diagram.el
utils/xwem-worklog.elc: lisp/xwem-load.el lisp/xwem-misc.el lisp/xwem-compat.el utils/xwem-diagram.el

dockapp/xwem-battery.elc: lisp/xwem-load.el
dockapp/xwem-framei.elc: lisp/xwem-load.el lisp/xwem-frame.el utils/xwem-osd.el
dockapp/xwem-pager.elc: lisp/xwem-load.el
dockapp/xwem-time.elc: lisp/xwem-load.el
dockapp/xwem-weather.elc: utils/xwem-osd.el lisp/xwem-interactive.el lisp/xwem-help.el lisp/xwem-compat.el

extra/xwem-edprops.elc: lisp/xwem-load.el
extra/xwem-framtrans.elc: lisp/xwem-load.el
extra/xwem-gamma.elc: lisp/xwem-load.el lisp/xwem-misc.el
extra/xwem-ratanot.elc: lisp/xwem-load.el lisp/xwem-manage.el
extra/xwem-recover.elc: lisp/xwem-load.el
extra/xwem-rooticon.elc: lisp/xwem-load.el lisp/xwem-misc.el
extra/xwem-smartmods.elc: lisp/xwem-load.el lisp/xwem-keyboard.el
extra/xwem-vert.elc: lisp/xwem-load.el

.PHONY: clean distclean ./lisp/xwem-version.el
