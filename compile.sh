#nasm interp.asm -f win32 -o interp.o && gcc interp.o -o interp.exe && ./interp.exe
nasm interp.asm -f elf64 -o interp.o && gcc interp.o -o interp.exe && ./interp.exe
