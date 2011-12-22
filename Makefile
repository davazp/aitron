.PHONY: clean

tron: tron.c
	gcc `pkg-config --libs --cflags sdl` $< -o $@

clean:
	rm tron
