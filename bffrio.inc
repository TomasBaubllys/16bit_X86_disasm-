; checks if we ran out of reading buffer
handle_buffer_in proc
	inc [current_address]
	inc si
	dec [buffer_in_size]
	cmp byte ptr ds:[buffer_in_size], 0				; check if we ran out of buffer
	ja _handle_buffer_in_return						; if buffer_size != 0, we cant continue printing 
	
	call read_buffer								; read more buffer
	
	_handle_buffer_in_return:
	
	; add current byte to byte buffer
	push ax bx
		mov bx, [byte_buffer_iter]
		mov al, byte ptr [si]
		call mov_byte_hex_byte_buffer 				
	pop bx ax
	
	ret 
endp

; flushes the buffer to stdout (called after each command read)
handle_buffer_out proc
	NEW_LINE_BUFFER_OUT
	push ax cx dx
		; print the adress from buffer out 
		mov cl, 07h                  
		mov bx, [output_file_handle] 
		lea dx, buffer_out
		mov ah, 40h
		int 21h
		
	
		;; print the bytes that were decoded
		mov bx, [byte_buffer_iter]
		mov byte ptr [bx], 'h'
		inc bx
		inc [byte_buffer_size]
		call fill_byte_buffer
		mov cl, BYTE_BUFFER_LEN ;[byte_buffer_size]
		lea dx, byte_buffer
		mov bx, [output_file_handle] ; 0001h
		mov ah, 40h
		int 21h
		;; magic ends here
		
		; print rest of the buffer
		mov cl, [buffer_out_size]
		sub cl, 07h
		mov bx, [output_file_handle] ; 0001h
		lea dx, buffer_out + 07h
		mov ah, 40h
		int 21h
		
		; prepare the buffer_out for the next iter
		mov [buffer_out_size], 0
		
		lea dx, byte_buffer
		mov [byte_buffer_iter], dx 
		mov [byte_buffer_size], 0
		pop dx cx ax
	ret
endp

; assumes byte buffer is in bx
fill_byte_buffer proc
	xor ch, ch
	mov cl, BYTE_BUFFER_LEN
	sub cl, [byte_buffer_size]
	
	fill_byte_buffer_loop:
		mov byte ptr [bx], 20h
		inc bx
	loop fill_byte_buffer_loop
	
	ret
endp

; swaps the last four bytes in bx in packs of two  34 12 $<- bx == 12 34  $<-bx
swap_last_4_packs_2 proc
	push bx cx ax
	dec bx						; bx is now set at the last number
	
	mov ax, word ptr [bx - 1]	; ah = 1, al = 2
	
	mov cx, word ptr [bx - 3]	; ch = 3 cl = 4
	
	mov [bx - 1], cx
	mov [bx - 3], ax
	
	pop ax cx bx
	ret
endp