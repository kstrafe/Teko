.PHONY:
all:
	./window.rkt
.PHONY:
nasm:
	nasm -f elf64 boot.asm -o boot.elf
	nasm boot_sector.asm -f bin -o boot_sector.bin
	qemu-system-x86_64 -drive format=raw,file=boot_sector.bin
