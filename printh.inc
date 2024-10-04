; assumes the number is in ax
print_dw_hex proc
	push ax													; save ax 

	; print ah
	shr ax, 8
	mov bh, 16
	div bh													; al / bh ah - remainder, al - quetient
	
	mov bl, ah												; save ah for later

	cmp al, 10
	jb _print_dw_hex_no_letter_1
	
	add al, 7

	_print_dw_hex_no_letter_1:
	mov dl, al
	add dl, '0'
	mov ah, 02h
	int 21h

	cmp bl, 10
	jb	_print_dw_hex_no_letter_2
	
	add bl, 7
	
	_print_dw_hex_no_letter_2:
	mov dl, bl
	add dl, '0'
	mov ah, 02h
	int 21h
	
	; now parse al
	pop ax
	and ax, 00FFh
	div bh
	
	mov bl, ah												; save ah for later

	cmp al, 10
	jb _print_dw_hex_no_letter_3
	
	add al, 7

	_print_dw_hex_no_letter_3:
	mov dl, al
	add dl, '0'
	mov ah, 02h
	int 21h

	cmp bl, 10
	jb	_print_dw_hex_no_letter_4
	
	add bl, 7
	
	_print_dw_hex_no_letter_4:
	mov dl, bl
	add dl, '0'
	mov ah, 02h
	int 21h

	ret 
endp


; assumes the number is in ax
print_dw_num proc
	push si dx 

	xor dx, dx 												; clear dx to keep the remainder
	mov bx, 10000											; set cx = 10000 to extract the first digit
	mov cx, 05h
	xor si, si												; si will be the flag for leading zeros
	mov si, 0

	get_digit:		
	div bx													; ax / cx ax - our digit, dx - remainder

	push dx												; save ax and dx because dx needs to be empty otherwise div by zero dince overflow

	cmp al, 0
	ja print_digit

	cmp si, 1
	je print_digit

	jmp next_iteration


	print_digit: 
	mov si, 1
	add al, '0'
	mov dl, al
	mov ah, 02h
	int 21h

	next_iteration:
	xor dx, dx

	mov ax, bx
	mov bx, 0Ah
	div bx
	mov bx, ax												; bx / 10

	pop dx
	mov ax, dx
	xor dx, dx

	loop get_digit

	pop dx si

	ret 
endp	
	