OBJ=asm.o
EXE=asm.exe
BENCHFILE=bench.fs
BENCHEXE=bench.exe
fsc=/suphys/jpal8929/FSharp-2.0.0.0/bin/fsc.exe
fscopts=--optimize+ --checked-
fsfile=initial.fs compiler.fs
fsexe=compiler.exe
asmfile=asm.asm
mono=/suphys/jpal8929/bin/mono
asm.exe: $(fsfile) 
	mono $(fsc) $(fscopts) $(fsfile) -g &&	mono --debug $(fsexe) > $(asmfile) && nasm $(asmfile) -DLIN64 -f elf64 -o $(OBJ) && gcc $(OBJ) -o $(EXE)

