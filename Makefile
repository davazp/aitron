.PHONY: clean

tron: tron.c
	gcc -o $@ $< `pkg-config --libs --cflags sdl`

clean:
	rm tron
