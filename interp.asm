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
;program db "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.",0
;factorial
program db    ">++++++++++>>>+>+[>>>+[-[<<<<<[+<<<<<]>>[[-]>[<<+>+>-]<[>+<-]<[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>[-]>>>>+>+<<<<<<-[>+<-]]]]]]]]]]]>[<+>-]+>>>>>]<<<<<[<<<<<]>>>>>>>[>>>>>]++[-<<<<<]>>>>>>-]+>>>>>]<[>++<-]<<<<[<[>+<-]<<<<]>>[->[-]++++++[<++++++++>-]>>>>]<<<<<[<[>+>+<<-]>.<<<<<]>.>>>>]", 0       ; don't forget nul terminator
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
array dq 0
jtab dq 0 ;stored in r9
plength dq 0

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
    ;allocate memory for memory
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
		je Allocjmptab
		jmp .loop_start

Allocjmptab:
    ;first calculate prog length
    mov rax, program
    dec rax
    xor rcx,rcx ;use rcx to hold prog length
    .loop_start:
        inc rax
        inc rcx
        cmp byte [rax],0
        je .loop_done
    .loop_done:
        mov rcx, [plength]
        ;now allocate memory
        mov vd, rcx
        sal vd,3 ;jtab has to store ints, so is 8 times bigger than the program, which is bytes
        call malloc
        mov[jtab], rax
        jmp Buildjmptab
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
        mov r9, [jtab]
		mov [r9 + rax*8], rcx
			;explaination - 
				;rax = position through program 
				;*8 - jump table is ints, program is bytes
				;jtab - need to add in base address
		;now do program.[curpointer] <- end(v)
		mov rax, rcx ;rax is now cur loc
		sub rax, program; rax now dist to current instruction
		pop rdx
		mov [r9 + rax*8],rdx 
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
	;decode at start should be faster - more time to get next opcode - but didn't actually make a difference
    inc rdx
	mov al, [rdx]
	
    mov [rbx], vd
	.update:
		add rbx, 8
		mov vd, [rbx]
	
	jmp decode
decdpC:
	inc rdx
	mov al, [rdx]
	mov [rbx], vd
	sub rbx, 8
	mov vd, [rbx]
		
	jmp decode
decbyteC:
	inc rdx
	mov al, [rdx]
	dec vd
	
	jmp decode
incbyteC:
	inc rdx
	mov al, [rdx]
	inc vd
	
	jmp decode
outputC:
	;works correctly in both oses - not sure if extra stack space necersarry, but keep it anyway 
    push rbx
	push vd
	push r8
    push r9
	push rdx
	call putchar
	pop rdx
    pop r9
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
	jmp .jmp ;for clarity
    .jmp:
    ;now we need to get the new value for instrpointer
	sub rdx, program
	mov rdx, [rdx*8+r9]
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
	mov rdx, [rdx*8+r9]
	;fall through
	.nojmp:
		inc rdx
		mov al, [rdx]
		jmp decode
;Instruction decode loop
;could be made cleverer - but probably safe to assume that all the opcodes are in memory
;best thing to do could be to map all the input chars to ints 1 - 8 and use those as offsets in a jump table - but this works	
decode:
	cmp al, [incdp]
	je incdpC
	cmp al, [decdp]
	je decdpC
	cmp al, [decbyte]
	je decbyteC
	cmp al, [incbyte]
	je incbyteC
	cmp al, [output]
	je outputC
	cmp al, [whilestart]
	je whilestartC
	cmp al, [whileend]
	je whileendC
	cmp al, 0 ;the input string is nul-terminated, so the final char will be 0 - note that this comparison is not necersarry as the fallthrough goes to the exit case
	je exit
;EXIT HERE
exit:
	xor rax,rax
	leave
    ret
