;The aim of this is to create a brainfuck intepreter
;status - working on loop instruction
extern _printf
extern _malloc
global _main

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

incdp dw ">"
decdp dw "<"
incbyte dw "+"
decbyte dw "-"
output dw "."
input dw ","
whilestart dw "["
whileend dw "]"
datapointer dd 0
instrpointer dd 0
array dd 0
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
_main:
	push 50000
	call _malloc
	add esp, 4
	mov [array], eax
	jmp Zeromem
;Here we zero the memory - this is necersarry because memory may not necersarrily be 0 at the start
Zeromem:
	mov ecx, 0 ;0 value
	mov eax,40004 ;
	add eax, [array]
	mov ebx, 0
	add ebx, [array]
	.loop_start:
		sub eax,4
		mov [eax], ecx
		cmp eax, ebx
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
	mov ecx, program
	dec ecx ;for loop to be easier
	jmp .loop_start
	.whilestart
		push ecx
		jmp .loop_start
	.whileend
		pop eax ;eax is v
		push eax ; use this again later
		sub eax, program ;eax is now count through program of original start tax
		;now I need to calculate the position to put the address
		lea ebx, [jtab + eax*4] 
			;explaination - 
				;eax = position through program 
				;*4 - jump table is ints, program is bytes
				;jtab - need to add in base address
		mov [ebx], ecx; does program.[v] <- curpointer
		
		;now do program.[curpointer] <- end(v)
		mov eax, ecx ;eax is now cur loc
		sub eax, program; eax now dist to current instruction
		lea ebx, [jtab + eax*4] ;need to add 4*eax as the jmp table is ints, whilst the program is chars (bytes)
								;ebx is now location in jump table of current instruction
								;now we need to work out where to jump to
		pop eax
		mov [ebx], eax
		jmp .loop_start
	.loop_start
		inc ecx
		mov al, [ecx]
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
;use this to print the memory - mainly useful as a debugging tool + me remembering how to write asm - not used currently
printmem:
	mov eax,-4
	add eax, array
	push eax
	.loop_start
		pop eax
		add eax, 4
		push eax
		mov ebx,4000
		add ebx,array
		cmp ebx, eax
		je initptrs
		mov ecx, [eax]
		push ecx
		push intf
		call _printf
		add esp,8 ;drop stack
		jmp .loop_start
		
initptrs:
	mov eax, program
	mov [instrpointer], eax
	mov ebx, [array]
	mov [datapointer], ebx
	jmp decode
	
	
;routines for different opcodes
incdpC:
	mov eax, [instrpointer]
	inc eax
	mov [instrpointer], eax
	
	mov eax, [datapointer]
	add eax,4
	mov [datapointer], eax
	
	jmp decode
decdpC:
	mov eax, [instrpointer]
	inc eax
	mov [instrpointer], eax
	
	mov eax, [datapointer]
	sub eax,4
	mov [datapointer], eax
	
	jmp decode
decbyteC:
	mov eax, [datapointer]
	mov ebx, [eax]
	dec ebx
	mov [eax], ebx
	
	mov eax, [instrpointer]
	inc eax
	mov [instrpointer], eax
	
	jmp decode
incbyteC:
	mov eax, [datapointer]
	mov ebx, [eax]
	inc ebx
	mov [eax], ebx
	
	mov eax, [instrpointer]
	inc eax
	mov [instrpointer], eax
	
	jmp decode
outputC:
	mov eax, [datapointer]
	push dword [eax]
	push charf
	call _printf
	add esp, 8
	
	mov eax, [instrpointer]
	inc eax
	mov [instrpointer], eax
	
	jmp decode
whilestartC:
	;check if memory.[pointer] = 0
	mov ebx, [datapointer]
	mov eax, [ebx]
	cmp eax, 0
	jne .nojmp
	;now we need to get the new value for instrpointer
	mov eax, [instrpointer]
	sub eax, program
	mov ebx, [eax*4+jtab]
	inc ebx
	mov [instrpointer], ebx
	jmp decode
	.nojmp 
		mov eax, [instrpointer]
		inc eax
		mov [instrpointer], eax
		jmp decode
whileendC:
	;check if memory.[pointer] = 0
	mov ebx, [datapointer]
	mov eax, [ebx]
	cmp eax, 0
	je .nojmp
	;now we need to get the new value for instrpointer
	mov eax, [instrpointer]
	sub eax, program
	mov ebx, [eax*4+jtab]
	inc ebx
	mov [instrpointer], ebx
	jmp decode
	.nojmp 
		mov eax, [instrpointer]
		inc eax
		mov [instrpointer], eax
		jmp decode
;Instruction decode loop
;could be made cleverer - but probably safe to assume that all the opcodes are in memory
;best thing to do could be to map all the input chars to ints 1 - 8 and use those as offsets in a jump table - but this works	
decode:
	mov ecx, [instrpointer]
	mov al, [ecx]
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


