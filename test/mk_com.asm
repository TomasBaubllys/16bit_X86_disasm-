.model small
	
.code
org    100H	

JUMPS	
	
begin:

	jmp main
	

main    proc    near        ; <=== Entry point (main function)
	ret 1
	ret 2
	
	je aaaa
	
	esc 3Fh, ax
	esc 2Ah, dx

	in ax, dx 
	out dx, al
	in al, dx
	in al, 03h
	out 0AAh, ax
	out 00h, al

	adc ax, [bp + di - 1]
	adc ax, [bp + di + 0FFFFh]
	sbb ax, 1010h
	sbb ax, 1010h
	sbb ax, 1010h
	sbb ax, 1010h
	sbb ax, 1010h
	adc [si], cx
	sbb [di + 8913h], dx
	add [bp + di + 8000h], dx
	sbb dx, [si + 8913h]
	sbb [di + 8913h], bp
	sbb bp, [di + 8913h]
	adc bp, 9000h
	adc ax, 0000h
	
	mov ds, ax
	
	hi2:
	loop hi2 
	
	push ds
	push ds
	push ds
	push ds
	push ds
	
	or ax, 90h
	add ah, 8h
	or al, 17h
	add ax, 1111h
	
	add ax, [bp + di + 8000h]
	add [bx + di], cx
	
	add cx, [si + 1]
	add [di], dx
	
	or cx, [si + 1]
	or [di], dx
	
	pop ds
	pop ss
	pop es
	
	aad 
	aam
	aam 
	xlat
	xlat
	
	movsb
	movsw
	stosb
	stosw
	lodsb
	lodsw
	scasb
	scasw
	test al, 09h
	test ax, 109h
	
	test ax, [bp + di]
	xchg ax, cx
	xchg bx, [si]
	
	lock
	repnz
	rep
	hlt
	cmc
	
	lea bx, [di]
	pop ax
	pop [bp + di + 099h]
	lea dx, [si]
	
	
	inc ax
	inc al
	
	test word ptr ds:[81h], 90h
	test cx, 77h
	test byte ptr [bp + di + 800h], 0Ah 
	
	inc byte ptr ds:[81h]
	dec bl
	dec word ptr [bp + di]
	jmp [bp+di + 8h]
	push [bp + di]
	pop ax
	
	std
	cld
	sti
	cli
	stc
	clc
	
	add cx, 09h
	or word ptr [bp + di + 900h], 9h
	adc bl, 3h
	sbb cx, 40h
	and cx, 19h
	sub word ptr [bp + di], 80h
	xor bx, 90h
	cmp word ptr [di], 900h
	
	nop
	xchg cx, ax
	cbw
	cwd
	call ss:[0984h]
	wait
	pushf
	popf
	sahf
	lahf
	
	not ax	
	not cl
	not byte ptr ds:[81h]
	not word ptr [bx + di + 8]
	
	mul dx
	neg ax
	neg byte ptr [si]
	
	idiv ax
	div byte ptr [bp +di]
	div word ptr [di + 90h]

	;mov dx, offset test_text
	mov ax, ds
	mov dl, 'A'
	mov ds, [bp + 300h]
	mov [bx + 01h], ds
	;mov ah, 02h
	int 21h
	
	mov byte ptr [di + 02h], 0004h
	mov word ptr [bp + di], 0004h
	
	;mov cx, bx
	mov dx, cx
	
	mov si, bx
	mov bx, si
	
	mov ax, 1
	mov bl, 09h
	
	;MOV AL, [BX+SI]
	;MOV BL, [BP+DI]
	mov [BP+DI], cl

	mov [si], ax
	jbe hi
	jc hi
	
	mov [bx + 01h], ax
	mov [bp + 333h], dx
	
	hi:

    ; Exit to DOS
    MOV ah, 4Ch
    MOV al, 0
    INT 21h
	
	mov ds, ax
	mov ds, ax
	mov ds, ax
	mov ds, ax
	mov ds, ax
	mov es, ax
	mov es, ax
	
	shr ax, cl
	shr ax, cl
	shr byte ptr [si + 90h], cl
	shr byte ptr [si], cl
	
	shr ax, cl
	shr ax, 07h
	shr byte ptr [si + 90h], 3
	shr byte ptr [si], cl
	
	rol byte ptr [si], 02h
	rcr cx, cl
	shl byte ptr [bp + 90h], 1
	
	
	das
	das
	das
	daa
	
	add byte ptr ds:[si], 09h
	add byte ptr es:[si], 09h
	add byte ptr cs:[78], 90h
	
	and dx, word ptr [bp + di + 12A3h]
	and word ptr [si], bp
	
	and ax, 09h
	and ax, 0FFh
	and ax, 0ECh
	and al, 00h
	
	sub al, 1
	sub al, 0CFh
	
	sub dx, ax
	sub byte ptr [si + 0FFh], ch
	
	cmp ax, 0AAh
	cmp bx, 0AAh
	cmp bp, word ptr [si + 33FCh]
	
	xor word ptr [di], cx
	xor ch, byte ptr [di]
	xor word ptr [bx + si + 0Ah], dx
	xor ah, 090h
	xor al, 0FFh
	
	push ds es ss ax bx bp
	
	aas
	
	aaa
	aaa
	aaa
	aaa
	
	aas
	
	retn
	retn
	
	retf
	int 3
	into
	iret
	
	les ax, es:[bx + di]
	lds dx, ss:[si + 900h]
	les cx, ds:[900]
	
	ret
	
	ret 1234h
	ret 0A1B2h
	ret 1
	
	retf 0ABCDh
	
	add cl, 01h
	add ah, 0FFh
	
	aaaa: db 001
	bbbb dw 0ffffh
	
main    endp                ;<=== End function
	
end begin                ;<=== End program


	;; tasm mk_com
	;; tlink /t mk_com
