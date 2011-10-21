MAKEINFO = makeinfo

all: xwem.info

xwem.info: xwem-version.texi xwem.texi
	$(MAKEINFO) xwem.texi

xwem-version.texi:
	if [ -d "../{arch}" -a -x `which tla 2>/dev/null` ]; then \
		printf "@set VERSION %s" `tla logs -f|tail -n1|sed s/@/@@/` > $@; \
	else \
		echo "@set VERSION $(VER)" > $@; \
	fi

.SUFFIXES: .info .texi

clean:
	rm -f xwem.info xwem-version.texi

.PHONY: clean xwem-version.texi
