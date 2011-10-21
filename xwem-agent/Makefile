CC=cc
CFLAGS=-Wall -g
INCLUDES=-I/usr/X11R6/include
LIBS=-L/usr/X11R6/lib -lX11

PROG=xwem-agent
OBJS=xwem-agent.o

$(PROG): $(OBJS)
	$(CC) $(CFLAGS) -o xwem-agent $(OBJS) $(LIBS)

xwem-agent.o: xwem-agent.c
	$(CC) $(CFLAGS) -c -o xwem-agent.o xwem-agent.c $(INCLUDES)

xwem-xmsg: xwem-xmsg.o
	$(CC) $(LDFLAGS) -o xwem-xmsg xwem-xmsg.o $(LIBS)

xwem-xmsg.o: xwem-xmsg.c
	$(CC) $(CFLAGS) -c -o xwem-xmsg.o xwem-xmsg.c $(INCLUDES)

.PHONY: clean

clean:
	rm -f $(OBJS) $(PROG) xwem-msg.o xwem-msg *~
