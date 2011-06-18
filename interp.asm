;The aim of this is to create a brainfuck intepreter
extern _printf
global _main
;
; initialized data is put in the .data segment
;
segment .data
;
; These labels refer to strings used for output
;
program db    "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.+++++++++++++++++++++++++++++.+++++++..+++.-------------------------------------------------------------------.------------.+++++++++++++++++++++++++++++++++++++++++++++++++++++++.++++++++++++++++++++++++.+++.------.--------.-------------------------------------------------------------------._", 0       ; don't forget nul terminator
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
endd dw "_"
;
; use bss to hold brainfuck program memory
;
segment .bss
;memory for program
array: resw 100
datapointer: resw 4 ;not sure why I need to reserve 4 words - reserving only 1 appears to be insufficient
instrpointer: resw 4

;
; code is put in the .text segment
;

segment .text
_main:
	jmp Zeromem
;Here we zero the memory	
Zeromem:
	mov ecx, 0 ;0 value
	mov eax,404
	add eax, array
	mov ebx, 0
	add ebx, array
	.loop_start:
		sub eax,4
		mov [eax], ecx
		cmp eax, ebx
		je initptrs
		jmp .loop_start

;use this to print the memory - mainly useful as a debugging tool + me remembering how to write asm
printmem:
	mov eax,-4
	add eax, array
	push eax
	.loop_start
		pop eax
		add eax, 4
		push eax
		mov ebx,400
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
	mov eax, array
	mov [datapointer], eax
	jmp decode
;routines for different opcodes
incdpC:
	mov eax, [instrpointer]
	inc eax
	mov [instrpointer], eax
	
	mov eax, [datapointer]
	inc eax
	mov [datapointer], eax
	
	jmp decode
decdpC:
	mov eax, [instrpointer]
	inc eax
	mov [instrpointer], eax
	
	mov eax, [datapointer]
	dec eax
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
	mov bl, [endd]
	cmp al, bl
	je exit
exit:
	;EXIT HERE
        ret


