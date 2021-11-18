.PHONY: all
all: safeio_impl.c

safeio_impl.c ../include/safeio.hrl: safeio.h
	sedge -i $< -c $@ -e ../include/safeio.hrl
