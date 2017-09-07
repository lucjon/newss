GAPROOT ?= ../..
CFLAGS += -fPIC -DPIC -DCONFIG_H -DHAVE_CONFIG_H -O3 -I$(GAPROOT) -I$(GAPROOT)/extern/install/libatomic_ops/include
OBJECTS = src/inner.o

src/inner.so: $(OBJECTS)
	$(CC) -shared $(CFLAGS) -o $@ $^ -lm -ldl -lutil -lgmp

src/%.o: src/%.c
	$(CC) -c $(CFLAGS) -o $@ $<

clean:
	rm -f $(OBJECTS) src/inner.so

.PHONY: clean
