mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfile_dir := $(dir $(mkfile_path))

all:
	dasm *.asm -f3 -v0 -oplayfield.bin -lplayfield.lst -splayfield.sym

run:
	stella playfield.bin

clean:
	rm *.bin *.lst *.sym

ecoa:
	echo $(mkfile_path)
	echo $(mkfile_dir)
