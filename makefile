FILE=interp.asm
OBJ=interp.o
EXE=interp.exe

windows: $(FILE)
	nasm $(FILE) -DWIN64 -f win64 -o $(OBJ) && /usr/bin/x86_64-w64-mingw32-gcc $(OBJ) -o $(EXE)
linux: $(FILE)
	nasm $(FILE) -DLIN64 -f elf64 -o $(OBJ) && gcc $(OBJ) -o $(EXE)
