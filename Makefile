GAPROOT ?= ../..
CFLAGS += -fPIC -DPIC -DCONFIG_H -DHAVE_CONFIG_H -g -O2 -I$(GAPROOT)

src/inner.so: src/inner.c
	$(CC) -shared $(CFLAGS) -o $@ $< -lm -ldl -lutil -lgmp

clean:
	rm -f src/inner.so

.PHONY: clean
