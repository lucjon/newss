GAPROOT ?= ../..
CFLAGS += -fPIC -DPIC -DCONFIG_H -DHAVE_CONFIG_H -g -O2 -I$(GAPROOT)
OBJECTS = src/inner.o

src/inner.so: $(OBJECTS)
	$(CC) -shared $(CFLAGS) -o $@ $^ -lm -ldl -lutil -lgmp

src/%.o: src/%.c
	$(CC) -c $(CFLAGS) -o $@ $<

clean:
	rm -f src/inner.so

.PHONY: clean
