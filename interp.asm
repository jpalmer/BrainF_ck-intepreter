;The aim of this is to create a brainfuck intepreter
;status - current error appears to be that the data is not being correctly saved - not sure what is going on with that 
extern printf
extern malloc
extern putchar
;due to different calling convention, the registers that we use depend on the OS
%ifdef WIN64
	%define vd rcx
    %define main WinMain ;on windows main is not called main
%endif
%ifdef LIN64
    %define vd rdi
%endif

global main

;x64 calling convention
;ints / pointers - RCX RDX R8 R9
;
; initialized data is put in the .data segment - it may be true that these are not modifiable
segment .data
;loopless hello world
;program db "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.+++++++++++++++++++++++++++++.+++++++..+++.-------------------------------------------------------------------.------------.+++++++++++++++++++++++++++++++++++++++++++++++++++++++.++++++++++++++++++++++++.+++.------.--------.-------------------------------------------------------------------.",0
;hello world w/ loops
program db "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.",0
;factorial
;program db    ">++++++++++>>>+>+[>>>+[-[<<<<<[+<<<<<]>>[[-]>[<<+>+>-]<[>+<-]<[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>[-]>>>>+>+<<<<<<-[>+<-]]]]]]]]]]]>[<+>-]+>>>>>]<<<<<[<<<<<]>>>>>>>[>>>>>]++[-<<<<<]>>>>>>-]+>>>>>]<[>++<-]<<<<[<[>+<-]<<<<]>>[->[-]++++++[<++++++++>-]>>>>]<<<<<[<[>+>+<<-]>.<<<<<]>.>>>>]", 0       ; don't forget nul terminator
debug db "dd",0
intf db "%i" ,10,0
charf db "%c",0
hellostr db "loop", 10 ,0
incdp db ">"
decdp db "<"
incbyte db "+"
decbyte db "-"
output db "."
input db ","
whilestart db "["
whileend db "]"
array dq 0,0,0,0,0,0,0,0,0,0,0 ;why not have some padding

segment .bss
jtab: resb 100000; should support programs up to 10k in length - 10k should be enough for anyone

segment .text
main:
%ifdef WIN64 ;not sure precisely what this does, got it by doing an objdump of a c exe - consider it magic
	push   rbp
   	mov    rbp,rsp
   	sub    rsp,0x20
%endif
%ifdef LIN64
    push rbp
    mov rbp,rsp
    sub rsp,0x10
%endif
	mov vd,  0x5000000
	call malloc
	mov [array], rax
	
	jmp Zeromem
;Here we zero the memory - this is necersarry because memory may not necersarrily be 0 at the start
;could possibly make this faster using SSE / mmx
Zeromem:
	mov rcx, 0 ;0 value to store in memory
	mov rax,40008 ;currently 5k memory cells
	add rax, [array] ;rax is 'end' of memory
	mov rbx, 0
	add rbx, [array];rbx is 'start' of memory
	.loop_start:
		sub rax,8
		mov [rax], rcx
		cmp rax, rbx
		je Buildjmptab
		jmp .loop_start

Buildjmptab:
	;Variables I need to store
		;I use the actual stack as the stack here - why not + frees up registers
		;rcx - position through program
	mov rcx, program
	dec rcx ;for loop to be easier
	jmp .loop_start
	.whilestart:
		push rcx
		jmp .loop_start
	.whileend:
		pop rax ;rax is v
		push rax ; use this again later
		sub rax, program ;rax is now count through program of original start tax
		;now I need to calculate the position to put the address
		mov [jtab + rax*8], rcx
			;explaination - 
				;rax = position through program 
				;*8 - jump table is ints, program is bytes
				;jtab - need to add in base address
		;now do program.[curpointer] <- end(v)
		mov rax, rcx ;rax is now cur loc
		sub rax, program; rax now dist to current instruction
		pop rdx
		mov [jtab + rax*8],rdx 
		jmp .loop_start
	.loop_start:
		inc rcx
		mov al, [rcx]
		mov dl, [whilestart]
		cmp dl,al
		je .whilestart
		mov dl, [whileend]
		cmp dl,al
		je  .whileend
		xor dl, dl ;move 0
		cmp dl,al
		jne .loop_start ;if not end, keep going
		jmp initptrs ;now go to main loop
		
initptrs:
	mov rdx, program
	mov al, [rdx]
	mov rbx, [array]
	mov vd, [rbx]
	jmp decode
	
	
;routines for different opcodes
incdpC:
	mov [rbx], vd
;fall through	
	.update:
		add rbx, 8
		mov vd, [rbx]
;fall through
	inc rdx
	mov al, [rdx]
	
	jmp decode
decdpC:
	mov [rbx], vd
	
	.update:
		sub rbx, 8
		mov vd, [rbx]
		
	inc rdx
	mov al, [rdx]
	jmp decode
decbyteC:
	dec vd
	
	inc rdx
	mov al, [rdx]
	jmp decode
incbyteC:
	inc vd
	
	inc rdx
	mov al, [rdx]
	jmp decode
outputC:
	;works correctly in both oses - not sure if extra stack space necersarry, but keep it anyway 
    push rbx
	push vd
	push r8
	push rdx
	sub rsp,32
	call putchar
	add rsp,32
	pop rdx
	pop r8
	pop vd
	pop rbx
	
	inc rdx
	mov al, [rdx]
	
	jmp decode
whilestartC:
	;check if memory.[pointer] = 0
	cmp vd, 0
	jne .nojmp
	jmp .jmp
    .jmp:
    ;now we need to get the new value for instrpointer
	sub rdx, program
	mov rdx, [rdx*8+jtab]
	;fall through
	.nojmp:
		inc rdx
		mov al, [rdx]
		jmp decode
	
whileendC:
	;check if memory.[pointer] = 0
	cmp vd, 0
	je .nojmp
	;now we need to get the new value for instrpointer
	sub rdx, program
	mov rdx, [rdx*8+jtab]
	;fall through
	.nojmp:
		inc rdx
		mov al, [rdx]
		jmp decode
;Instruction decode loop
;could be made cleverer - but probably safe to assume that all the opcodes are in memory
;best thing to do could be to map all the input chars to ints 1 - 8 and use those as offsets in a jump table - but this works	
decode:
	mov bl, [incdp]
	cmp al, bl
	je incdpC
	mov bl, [decdp]
	cmp al, bl
	je decdpC
	mov bl, [decbyte]
	cmp al, bl
	je decbyteC
	mov bl, [incbyte]
	cmp al, bl
	je incbyteC
	mov bl, [output]
	cmp al, bl
	je outputC
	mov bl, [whilestart]
	cmp al, bl
	je whilestartC
	mov bl, [whileend]
	cmp al, bl
	je whileendC
	mov bl, 0 ;the input string is nul-terminated, so the final char will be 0 - note that this comparison is not necersarry as the fallthrough goes to the exit case
	cmp al, bl
	je exit
;EXIT HERE
exit:
	xor rax,rax
	leave
    ret
