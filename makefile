FILE=interp.asm
OBJ=interp.o
EXE=interp.exe
BENCHFILE=bench.fs
BENCHEXE=bench.exe
fsc=~/FSharp-2.0.0.0/bin/fsc.exe
fscopts=--optimize+ --checked-
fsfile=Fsharp/fsharp_interp.fs
mono=mono
fsharp: $(fsfile)
	mono $(fsc) $(fscopts) $(fsfile)
windows: $(FILE)
	nasm $(FILE) -DWIN64 -f win64 -o $(OBJ) && /usr/bin/x86_64-w64-mingw32-gcc $(OBJ) -o $(EXE)
linux: $(FILE)
	nasm $(FILE) -DLIN64 -f elf64 -o $(OBJ) && gcc $(OBJ) -o $(EXE)
asmcompiler: compiler/compiler.fs
	cd compiler && make && cp asm.exe ..
benchmark: $(BENCHFILE) asmcompiler
	$(mono) $(fsc) $(BENCHFILE) -o $(BENCHEXE) && $(mono) $(BENCHEXE)
