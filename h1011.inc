; assumes the byte is in al, si (buffer_in pointer) gets changed around must be played with carefully!!!
print_1011_call proc
	push cx ax bx
	
	xor ch, ch											; this is not needed if we are sure the value is B...h
	mov cl, OPC_MOV_REG_IMM_EL_COUNT				
	lea di, ds:[opcode_table_mov_reg_imm8]				; load the beginning of our table
	
	; look for the value in our opcode table
	_look_for_mov_imm_val:
		cmp al, byte ptr [di]
		je print_opcode								; if we find our value print it CHANGE THIS 

		add di, OPC_MOV_REG_IMM_EL_LEN				; move to the next element in our table
		loop _look_for_mov_imm_val					; loop until we find it
	
	print_opcode: 									; if we found the opcode print it

	; copy_to_ouput_buffer
	lea bx, buffer_out								; use bx as our iterator, saves a lot of moving around, but buffer_out_iter is still needed for parsing bytes individually
	mov ax, [current_address]
	
	call mov_word_hex_buffer_out
	
	COLON_BUFFER_OUT								; sourceeeeee
	WHITE_SPACE_BUFFER_OUT
	
	push di
	lea di, mov_str
	mov cx, 0003h
	call move_command_to_bffr
	pop di
	
	xor ch, ch
	mov cl, OPC_MOV_REG_IMM_EL_LEN - 1				; -1 because first byte is the code so we need to copy one byte less
	add di, 1										; move to the beginning of the opcode
	
	_copy_buffer_in_out_mov_reg_imm:
		mov al, byte ptr [di]
		mov [bx], al
		inc di
		inc [buffer_out_size]						; increment buffer size
		inc bx
	loop _copy_buffer_in_out_mov_reg_imm
	
	mov [buffer_out_iter], bx						; save the buffer_out_current pos for now
	
	; then print the next two bytes
	inc [current_address]
	cmp byte ptr [di], 184							; compare if its < B8h, not print the second byte
	jb	_skip_one_mov_imm							
	
	inc si											; move to the next byte		
	inc [current_address]
	
	call handle_buffer_in_ch							; handle the buffer

	dec [buffer_in_size]						; decrement our byte stream size SHOULD BE + 1 to account for big endian
	mov al, byte ptr[si]						; print the next bytes value 
	call mov_byte_hex_buffer_out

	
	_skip_one_mov_imm:
		inc si			
		call handle_buffer_in_ch
	
		dec [buffer_in_size]	
		mov al, byte ptr[si]					; SHOULD BE -1 to account for big endian
		call mov_byte_hex_buffer_out
		
	mov bx, [buffer_out_iter]					; sort this out plz!!! 	
	NEW_LINE_BUFFER_OUT
	call handle_buffer_out
	
	pop bx ax cx
	ret
endp