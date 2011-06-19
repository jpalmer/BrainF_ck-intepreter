#nasm interp.asm -f win32 -o interp.o && gcc interp.o -o interp.exe && ./interp.exe
nasm interp.asm -f win64 -o interp.o && /usr/bin/x86_64-w64-mingw32-gcc interp.o -o interp.exe && ./interp.exe
