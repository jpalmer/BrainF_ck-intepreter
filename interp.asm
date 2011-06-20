;The aim of this is to create a brainfuck intepreter
;status - working on loop instruction
extern printf
extern malloc
extern puts
extern putchar
global main
;x64 calling convention - windows
;ints / pointers - RCX RDX R8 R9
;
;x64 calling convention - linux
;rdi, rsi, rdx, rcx, r8, r9,;


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
datapointer dq 0
instrpointer dq 0
array dq 0
;
; use bss to hold brainfuck program memory
;
segment .bss
;memory for program
;array: resw 10 ;if you change this need to change in zeromem and printmem routines
jtab: resb 100000; should support programs up to 10k in length - 10k should be enough for anyone

;
; code is put in the .text segment
;

segment .text
main:
    sub rsp, 8
    mov rdi,  50000
    call malloc
    add rsp, 8
    mov [array], rax
	jmp Zeromem
;Here we zero the memory - this is necersarry because memory may not necersarrily be 0 at the start
;could possibly make this faster using SSE / mmx
Zeromem:
	mov rcx, 0 ;0 value
	mov rax,40008 ;
	add rax, [array]
	mov rbx, 0
	add rbx, [array]
	.loop_start:
		sub rax,8
		mov [rax], rcx
		cmp rax, rbx
		je Buildjmptab
		jmp .loop_start

;let fixwhiles (program:chars[]) =
 ;   let startstack = System.Collections.Generic.Stack<_>()
  ;  let mutable curpointer = -1
   ; while curpointer+1 < program.Length do
    ;    curpointer <- curpointer+1
     ;   match program.[curpointer] with
      ;  |Whilestart(_) -> startstack.Push(curpointer)
       ; |Whileend(_) ->
        ;    let v = startstack.Pop()
         ;   program.[v] <- Whilestart(curpointer)
          ;  program.[curpointer] <- Whileend(v)
        ;|_ -> () //do nothing for other stuff
    ;program
Buildjmptab:
	;Variables I need to store
		;I use the actual stack as the stack here - why not + frees up registers
		;ecx - position through program
		;a,b,d are free for other things - 64bit would be really nice for the extra registers here
	mov rcx, program
	dec rcx ;for loop to be easier
	jmp .loop_start
	.whilestart:
		push rcx
		jmp .loop_start
	.whileend:
		pop rax ;eax is v
		push rax ; use this again later
		sub rax, program ;eax is now count through program of original start tax
		;now I need to calculate the position to put the address
		lea rbx, [jtab + rax*8] 
			;explaination - 
				;eax = position through program 
				;*4 - jump table is ints, program is bytes
				;jtab - need to add in base address
		mov [rbx], rcx; does program.[v] <- curpointer
		
		;now do program.[curpointer] <- end(v)
		mov rax, rcx ;eax is now cur loc
		sub rax, program; eax now dist to current instruction
		lea rbx, [jtab + rax*8] ;need to add 4*eax as the jmp table is ints, whilst the program is chars (bytes)
								;ebx is now location in jump table of current instruction
								;now we need to work out where to jump to
		pop rax
		mov [rbx], rax
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
		mov dl, 0
		cmp dl,al
		jne .loop_start
		jmp initptrs ;now go to main loop
		
initptrs:
	mov rax, program
	mov [instrpointer], rax
	mov rbx, [array]
	mov [datapointer], rbx
	jmp decode
	
	
;routines for different opcodes
incdpC:
	mov rax, [instrpointer]
	inc rax
	mov [instrpointer], rax
	
	mov rax, [datapointer]
	add rax,8
	mov [datapointer], rax
	
	jmp decode
decdpC:
	mov rax, [instrpointer]
	inc rax
	mov [instrpointer], rax
	
	mov rax, [datapointer]
	sub rax,8
	mov [datapointer], rax
	
	jmp decode
decbyteC:
	mov rax, [datapointer]
	mov rbx, [rax]
	dec rbx
	mov [rax], rbx
	
	mov rax, [instrpointer]
	inc rax
	mov [instrpointer], rax
	
	jmp decode
incbyteC:
	mov rax, [datapointer]
	mov rbx, [rax]
	inc rbx
	mov [rax], rbx
	
	mov rax, [instrpointer]
	inc rax
	mov [instrpointer], rax
	
	jmp decode
outputC:
	mov rax, [datapointer]
	mov rdi, [rax]
    call putchar
	mov rax, [instrpointer]
	inc rax
	mov [instrpointer], rax
	
	jmp decode
whilestartC:
	;check if memory.[pointer] = 0
	mov rbx, [datapointer]
	mov rax, [rbx]
	cmp rax, 0
	jne .nojmp
	;now we need to get the new value for instrpointer
	mov rax, [instrpointer]
	sub rax, program
	mov rbx, [rax*8+jtab]
	inc rbx
	mov [instrpointer], rbx
	jmp decode
	.nojmp :
		mov rax, [instrpointer]
		inc rax
		mov [instrpointer], rax
		jmp decode
whileendC:
	;check if memory.[pointer] = 0
	mov rbx, [datapointer]
	mov rax, [rbx]
	cmp rax, 0
	je .nojmp
	;now we need to get the new value for instrpointer
	mov rax, [instrpointer]
	sub rax, program
	mov rbx, [rax*8+jtab]
	inc rbx
	mov [instrpointer], rbx
	jmp decode
	.nojmp:
		mov rax, [instrpointer]
		inc rax
		mov [instrpointer], rax
		jmp decode
;Instruction decode loop
;could be made cleverer - but probably safe to assume that all the opcodes are in memory
;best thing to do could be to map all the input chars to ints 1 - 8 and use those as offsets in a jump table - but this works	
decode:
	mov rcx, [instrpointer]
	mov al, [rcx]
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
exit:
	;EXIT HERE
        ret


