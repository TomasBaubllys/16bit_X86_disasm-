; checks if we ran out of reading buffer
handle_buffer_in proc
	cmp byte ptr ds:[buffer_in_size], 0				; check if we ran out of buffer
	ja _handle_buffer_in_return						; if buffer_size != 0, we cant continue printing 
	
	call read_buffer								; read more buffer
	
	_handle_buffer_in_return:
	ret 
endp

; flushes the buffer to stdout (called after each command read)
handle_buffer_out proc
	push ax bx cx
		mov cl, [buffer_out_size]
		mov bx, 0001h
		lea dx, buffer_out
		mov ah, 40h
		int 21h
	pop cx bx ax
		; prepare the buffer_out for the next iter
		mov [buffer_out_size], 0
		
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