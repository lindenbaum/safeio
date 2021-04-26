ERL_TOP ?= /usr/lib64/erlang/

SAFEIO_CFLAGS := -Wno-unused-parameter

CFLAGS := -W -Wall -g $(SAFEIO_CFLAGS) -I $(ERL_TOP)/usr/include/
LDFLAGS := -rdynamic -L$(ERL_TOP)/usr/lib/
LDLIBS := -lei

.PHONY: all
all: safeio_port
	mkdir -p ../priv
	cp $^ ../priv/

.PHONY: clean
clean:
	$(RM) -r ../priv
	$(RM) *.o *.d *.so safeio_port

safeio_port: safeio_impl.c safeio_port.c safeio.c
	$(CC) -o $@ $(CFLAGS) $^ $(LDFLAGS) $(LDLIBS)

safeio_impl.c ../include/safeio.hrl: safeio.h
	sedge -i $< -c $@ -e ../include/safeio.hrl
