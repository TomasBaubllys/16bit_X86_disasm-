; Programa: Nr. 3
; Užduoties sąlyga: Apdoroti:mov, out, not, rcr, xlat
; Atliko: Tomas Baublys 

;; TODO :
;;
;;
;;	> Optimize opcode table


.model small
.stack 100h

include macrod.inc

JUMPS																		; for conditional long jumps

.data
	output_file db 13 dup(?) ;'rez.asm', '$'
	output_file_handle dw ?
	
	newln db 0Dh, 0Ah, '$'

	input_file db MAX_FILE_NAME_LEN dup(?)							; file name from which we are going to read DTA 8.3 format 
	input_file_handle dw ?
	
	cannot_open_file db "Error while openning file!"
					cof_err_len equ $ - cannot_open_file
	
	eof_success db "Disassembly complete. End of file reached successfully.", '$'

	buffer_in_size db ?
	buffer_in db BUFFER_IN_LEN dup(?)
	
	current_address dw 0000h
	
	buffer_out_size db ?
	buffer_out db BUFFER_OUT_LEN dup(?)
	
	help_msg db 'Usage: diss [output_file] [input_file]', 0Dh, 0Ah
			db 'Disassembles 16-bit 8086 machine code.', 0Dh, 0Ah
			db 'Example: diss output.asm input.com', 0Dh, 0Ah
			db ' - output_file: The file to write the disassembly to.', 0Dh, 0Ah
			db ' - input_file: The binary file to disassemble.', 0Dh, 0Ah
			db 0
	help_msg_len equ $ - help_msg
	
	_d db ?
	_w db ?
	_mod dw ?
	
	; for mov reg, imm8/imm16
	opcode_table_std_breg_nr db 00h, 'al'
						db 01h, 'cl'
						db 02h, 'dl'
						db 03h, 'bl'
						db 04h, 'ah'
						db 05h, 'ch'
						db 06h, 'dh'
						db 07h, 'bh'
	opcode_table_std_wreg_nr db 08h, 'ax'
						db 09h, 'cx'
						db 0Ah, 'dx'
						db 0Bh, 'bx'
						db 0Ch, 'sp'
						db 0Dh, 'bp'
						db 0Eh, 'si'
						db 0Fh, 'di'
						
	mov_str db 'mov '
	mov_str_len equ $ - mov_str
	
	unknown_str db 'unknown '
	unknown_str_len equ $ - unknown_str
	
	int_str db 'int '
	int_str_len equ $ - int_str
	
	opcode_0111 db 70h, 04h, 'jo '
				db 71h, 05h, 'jno '
				db 72h, 04h, 'jc '
				db 73h, 05h, 'jae '
				db 74h, 04h, 'je '
				db 75h,	05h, 'jne '
				db 76h, 05h, 'jbe '
				db 77h, 04h, 'ja '
				db 78h, 05h, 'js '
				db 79h, 05h, 'jns '
				db 7Ah, 04h, 'jp '
				db 7Bh, 05h, 'jnp '
				db 7Ch, 04h, 'jl '
				db 7Dh, 05h, 'jge '
				db 7Eh, 05h, 'jle '
				db 7Fh, 04h, 'jg '
				
	opcode_table_reg_mod00 db 00h, 09h, '[bx + si'
							   db 01h, 09h, '[bx + di'
							   db 02h, 09h, '[bp + si'
							   db 03h, 09h, '[bp + di'
							   db 04h, 04h, '[si'
							   db 05h, 04h, '[di'
							   db 06h, 02h, 0
							   db 07h, 04h, '[bx'
							   
	
	opcode_table_sr db 00h, 'es'
					db 01h, 'cs'
					db 02h, 'ss'
					db 03h, 'ds'
	
	opcode_1111_011 db 02h, 04h, 'not'
					db 03h, 04h, 'neg'
					db 04h, 04h, 'mul'
					db 05h, 05h, 'imul'
					db 06h, 04h, 'div'
					db 07h, 05h, 'idiv'
					
					
	opcode_1111_111 db 00h, 04h, 'inc'
					db 01h, 04h, 'dec'
					db 02h, 05h, 'call'
					db 03h, 05h, 'call'
					db 04h, 04h, 'jmp'
					db 05h, 04h, 'jmp'
					db 06h, 05h, 'push'
					
	opcode_0100 db 00h, 04h, 'inc'
				db 01h, 04h, 'dec'
				
				
	opcode_0101 db 00h, 05h, 'push'
				db 01h, 04h, 'pop'
				
	_bp_ db 00, 04h, '[bp' 
				
.code
include printh.inc
include hbyte.inc
include bffrio.inc

start:
	; load the data segment
	mov ax, @data
	mov es, ax
	
	mov si, 81h														; load the beginning of cmd args
	call skip_spaces												; skip spaces

	mov al, byte ptr ds:[si]
	cmp al, 13														; compare with a new line -> argc == 0
	je help
	
	mov ax, word ptr ds:[si]
	cmp ax, 3F2Fh													; compare with /?
	je help

	lea di, es:[output_file]
	call read_filename
	cmp byte ptr es:[output_file], '$'								; if the files name was empty print help msg
	je help 						
	
	lea di, input_file
	call read_filename
	
	; move the data segment back to the ds
	mov ax, es
	mov ds, ax
	
	; open file name
	lea dx,  ds:[output_file]
	mov ax, 3C00h
	mov cx, 00h
	int 21h
	jc print_error
	mov ds:[output_file_handle], ax


	; open the file
	mov ah, 3Dh
	mov al, 00h														; read only
	lea dx, input_file
	int 21h
	jc print_error
	
	; save the file hande
	mov word ptr [input_file_handle], ax
	
	read_buffer_jmp:
		call read_buffer
	
	;; make a tree later for now compare ok!!!
	parse_buffer:
		mov al, byte ptr [si]	; load the byte for comparision
		
		mov bl, al
		shr bl, 4				; check if the call was Bxh
		cmp bl, 0Bh
		je	handle_1011_l	 

		cmp bl, 07h
		je handle_0111_l
		
		cmp bl, 08h
		je handle_1000_l
		
		cmp bl, 0Ah
		je handle_1010_l
		
		cmp bl, 0Ch
		je handle_1100_l
		
		cmp bl, 0Fh
		je handle_1111_l
		
		cmp bl, 04h
		je handle_0100_l
		
		cmp bl, 05h 
		je handle_0101_l
		
		jmp _continue_loop
		
		handle_1011_l:
		call handle_1011
		jmp _continue_loop
		
		handle_0111_l:
		call handle_0111
		jmp _continue_loop
		
		handle_1000_l:
		call handle_1000
		jmp _continue_loop
		
		handle_1010_l:
		call handle_1010
		jmp _continue_loop
		
		handle_1100_l:
		call handle_1100
		jmp _continue_loop
		
		handle_0100_l:
		call handle_0100
		jmp _continue_loop
		
		handle_0101_l:
		call handle_0101
		jmp _continue_loop
		
		handle_1111_l:
		call handle_1111
		
		_continue_loop:
			call handle_buffer_in
			
	jmp parse_buffer
	

	jmp exit
	
help:
	mov ax, es
	mov ds, ax
	
	lea dx, ds:[help_msg]
	mov cx, help_msg_len
	mov bx, 0002h					; stderr
	mov ah, 40h
	int 21h
	jmp exit
	
eof_reached:							; end of file reached
	mov bx, [input_file_handle]			; close the input_file
	mov ah, 3Eh			
	int 21h	

	lea dx, eof_success
	mov ah, 09h
	int 21h
	jmp exit
	
print_error:						; cannot open file
	mov bl, 2
	xor ch, ch
	mov cl, cof_err_len
	lea dx, cannot_open_file
	mov ah, 40h
	int 21h
	jmp exit	
	
exit:
	; close output file
	mov bx, [output_file_handle]
	mov ah, 3Eh
	int 21h

    ; Exit to DOS
    mov ax, 4C00h
    INT 21h
	
;	======================
;		Functions area
;	======================

skip_spaces proc
	skip_spaces_loop:
		cmp byte ptr ds:[si], ' '
		jne skip_spaces_end
		inc si
		jmp skip_spaces_loop
	skip_spaces_end:
		ret
endp

read_filename proc
	push	ax
	call	skip_spaces
	read_filename_start:
		cmp	byte ptr ds:[si], 13		; compare with a new line
		je	read_filename_end			; if yes we are done
		cmp	byte ptr ds:[si], ' '		; if not white spaces, continue reading
		jne	read_filename_next			
	read_filename_end:
		mov	al, '$'						; write $ to the end
		stosb                           ; Store AL at address ES:(E)DI, di = di + 1
		pop	ax
		ret
	read_filename_next:
		lodsb							; mov ds:si -> al
		stosb                           ; Store AL at address ES:(E)DI, di = di + 1
		jmp read_filename_start
endp

;; expects current byte in al
handle_0111 proc
	push cx ax bx
	
	; find the coresponding conditional jump
	lea di, ds:[opcode_0111]
	push cx
	mov cx, OPC_0111_COUNT
	_handle_0111_look_up:
		cmp byte ptr [di], al
		loop _handle_0111_continue
		
		call move_di_scnd_byte
		
		jmp _handle_0111_look_up
	
	; opcode found
	_handle_0111_continue:
	pop cx
	lea bx, buffer_out
	mov ax, [current_address]
	
	call mov_word_hex_buffer_out					; print the current address
	
	COLON_BUFFER_OUT								; sourceeeeee
	WHITE_SPACE_BUFFER_OUT
	
	call move_di_to_bx_scnd_byte					; copy conditional jump call to buffer
	
	call handle_buffer_in						; check if we ran out of buffer_in
	
	mov al, byte ptr [si]	
	call mov_byte_hex_buffer_out					; move the jump address to the buffer
	
	NEW_LINE_BUFFER_OUT								; add a new line
	
	call handle_buffer_out							; flush the buffer to stdout
	
	pop bx ax cx
	ret
endp

; moves di n times specified by di, -> val, n, val, ...
move_di_scnd_byte proc
	push ax
	inc di
	mov al, byte ptr [di]
	xor ah, ah
	add di, ax
	pop ax
	ret
endp 

; moves di n - 1 bytes to bx, di -> val, n, val, ...
move_di_to_bx_scnd_byte proc
	push ds es ax si
	mov cl, byte ptr [di + 1]
	add di, 2
	
	xor ch, ch									; move one byte less 
	dec cx
	push cx
	
	mov ax, ds
	mov es, ax

	mov ax, di
	mov si, ax
	mov di, bx
	
	rep movsb
	
	pop cx
	add bx, cx								; preserve our original pointer to buffer_out
	add [buffer_out_size], cl
		
	pop si ax es ds
	ret
endp

; reads buffer into buffer, saves size in buffer_size, points si to the beginning of the buffer, cl - bytes read
read_buffer proc
	push bx ax
	mov bx, [input_file_handle]
	xor cx, cx
	mov cx, BUFFER_IN_LEN
	lea dx, buffer_in
	mov ah, 3Fh
	int 21h
	
	cmp ax, 0
	je eof_reached				; only exit from this 
	
	mov [buffer_in_size], al		; save buffer size
	
	xor ch, ch
	mov cl, [buffer_in_size]
	lea si, buffer_in
	pop ax bx
	ret
endp

; assumes the byte is in al FIX NAMING  
mov_byte_hex_buffer_out proc
	push cx ax
	
	xor ah, ah
	mov cl, 10h
	div cl
	
	push ax 				; save the remainder

	cmp al, 0Ah
	jb _print_first_hex
	
	add al, 7				; add if its a letter
	
	_print_first_hex:
		add al, '0'
		mov byte ptr [bx], al
		inc bx
		inc [buffer_out_size]
	
	pop ax
	
	mov al, ah 
	cmp al, 0Ah
	jb _print_second_hex
	
	add al, 7
	
	_print_second_hex:
		add al, '0'
		mov byte ptr [bx], al
		inc bx
		inc [buffer_out_size]

	pop ax cx
	
	ret
endp

;; assumes word is in ax and pointer is in bx (NEEDS OPTIMIZATION)
mov_word_hex_buffer_out proc
	push ax												; save ax 

	; print ah
	shr ax, 8
	mov dh, 16
	div dh													; al / bh ah - remainder, al - quetient
	
	mov dl, ah												; save ah for later

	cmp al, 10
	jb _mov_dw_hex_no_letter_1
	
	add al, 7

	_mov_dw_hex_no_letter_1:
	add al, '0'
	mov byte ptr [bx], al
	inc bx
	inc [buffer_out_size]

	cmp dl, 10
	jb	_mov_dw_hex_no_letter_2
	
	add dl, 7
	
	_mov_dw_hex_no_letter_2:
	add dl, '0'
	mov byte ptr [bx], dl
	inc bx
	inc [buffer_out_size]

	; now parse al
	pop ax
	and ax, 00FFh
	div dh
	
	mov dl, ah												; save ah for later

	cmp al, 10
	jb _mov_dw_hex_no_letter_3
	
	add al, 7

	_mov_dw_hex_no_letter_3:
	add al, '0'
	mov byte ptr [bx], al
	inc bx
	inc [buffer_out_size]

	cmp dl, 10
	jb	_mov_dw_hex_no_letter_4
	
	add dl, 7
	
	_mov_dw_hex_no_letter_4:	
	add dl, '0'
	mov byte ptr [bx], dl
	inc bx
	inc [buffer_out_size]

	ret 

endp
	
; expects di to be pointer to the move command, bx buffer_out pointer, cx command_len
move_command_to_bffr proc
	push ax
	_move_command_to_bffr_loop:
		mov al, byte ptr [di]
		mov byte ptr [bx], al
		inc bx
		inc [buffer_out_size]
		inc di
	loop _move_command_to_bffr_loop
	pop ax
	ret 
endp 
	
; assumes the byte is in al, si (buffer_in pointer) gets changed around must be played with carefully!!!
handle_1011 proc
	push cx ax bx
	
	and al, 0Fh											; mask to get the registers byte
	xor ch, ch											; this is not needed if we are sure the value is B...h
	mov cl, OPC_REG_COUNT				
	lea di, ds:[opcode_table_std_breg_nr]				; load the beginning of our table
	
	; look for the value in our opcode table
	_handle_1011_look_up_reg:
		cmp al, byte ptr [di]
		je _handle_1011_reg_fnd							; if we find our value print it CHANGE THIS 

		add di, OPC_REG_NAME_LEN						; move to the next element in our table
		loop _handle_1011_look_up_reg					; loop until we find it
	
	_handle_1011_reg_fnd: 								; if we found the opcode print it

	; copy_to_ouput_buffer
	lea bx, buffer_out									; use bx as our iterator, saves a lot of moving around, but buffer_out_iter is still needed for parsing bytes individually
	mov ax, [current_address]
	
	; move [address: ] to buffer_out
	call mov_word_hex_buffer_out
	COLON_BUFFER_OUT						
	WHITE_SPACE_BUFFER_OUT
	
	; move the command name [mov] to buffer_out
	push di
	lea di, mov_str
	mov cx, mov_str_len
	call move_command_to_bffr
	pop di
	
	; move the registers name [ ax, ] to buffer_out 
	mov cx, OPC_REG_NAME_LEN - 1
	call move_cxdi_to_bx
	
	; add a comma and space
	COMMA_BUFFER_OUT
	WHITE_SPACE_BUFFER_OUT
	
	mov dl, byte ptr [di]
	shr dl, 3
	and dl, 01h
	mov byte ptr [_w], dl
	
	call handle_bojb_bovb
	
	_handle_1011_flush:
	NEW_LINE_BUFFER_OUT
	call handle_buffer_out
	pop bx ax cx
	ret
endp	

; assumes the byte is in al
handle_1000 proc
	push dx cx
	
	lea bx, buffer_out
	
	; mov current address into buffer out
	push ax dx
	mov ax, [current_address]
	call mov_word_hex_buffer_out
	COLON_BUFFER_OUT
	WHITE_SPACE_BUFFER_OUT
	pop dx ax
	
	; check if the call was 1000_10dw
	mov dl, al
	and dl, 0Ch
	cmp dl, 08h
	je _handle_1000_10_l
	
	;; check if the call was other
	mov dl, al
	and dl, 0Dh
	cmp dl, 0Ch
	je _handle_1000_1000_11x0_l
	
	_handle_1000_1000_11x0_l:
	call handle_1000_11x0
	jmp _handle_1000_ret
	
	_handle_1000_10_l:
	call handle_1000_10
	jmp _handle_1000_ret
	
	_handle_1000_ret:
	
	pop cx dx
	ret
endp 

handle_1000_11x0 proc
	push cx
	
	; move "mov" to buffer_out
	push di cx
	lea di, mov_str
	mov cx, mov_str_len
	call move_command_to_bffr
	pop cx di
	
	; extract the direction byte from al
	push ax
	and al, 02h
	shr al, 1
	mov byte ptr [_d], al
	pop ax
	
	mov byte ptr [_w], 01h				; in this case we only operate with words
	
	; move to the next byte 
	call handle_buffer_in
	mov al, byte ptr [si]				; load the next byte
	
	; fill _mod
	push ax
	shr al, 6
	mov byte ptr [_mod], al
	pop ax
	
	cmp byte ptr [_d], 1				; 
	jne handle_1000_11x0_d1
	;; extract _sr
	push ax
	and al, 18h
	shr al, 3

	call mov_sr_to_bx
	;; call to print _sr
	
	COMMA_BUFFER_OUT
	WHITE_SPACE_BUFFER_OUT
	
	pop ax
	and al, 07h
	call move_regmem_to_bx
	
	jmp handle_1000_11x0_exit
	handle_1000_11x0_d1:
	
	push ax 								; save al
	; extract reg/mem
	and al, 07h
	call move_regmem_to_bx
	
	COMMA_BUFFER_OUT
	WHITE_SPACE_BUFFER_OUT
	
	; extract _sr
	pop ax
	and al, 18h
	shr al, 3
	call mov_sr_to_bx
	
	handle_1000_11x0_exit:
	NEW_LINE_BUFFER_OUT
	call handle_buffer_out
	pop cx
	ret 
endp

; assumes sr is in al
mov_sr_to_bx proc
	lea di, ds:[opcode_table_sr]
	mov_sr_to_bx_look_up:
		cmp al, byte ptr [di]
		je mov_sr_to_bx_load_op
		
		add di, OPC_SR_NAME_LEN
		
		jmp mov_sr_to_bx_look_up
	
	; once we find it load it to bx
	mov_sr_to_bx_load_op:
	mov cx, OPC_SR_NAME_LEN - 1
	call move_cxdi_to_bx	
	ret
endp 

; handles mov reg <-> mem/reg
handle_1000_10 proc
	push cx dx
	
	; move "mov" to buffer_out
	push di cx
	lea di, mov_str
	mov cx, mov_str_len
	call move_command_to_bffr
	pop cx di

	; fill out the values _d, _w, _mod
	mov al, byte ptr [si]
	
	mov cl, al
	and al, 02h
	shr al, 1
	mov byte ptr [_d], al
	
	mov al, cl
	and al, 01h
	mov byte ptr [_w], al
	
	; move to the next byte
	call handle_buffer_in
	
	; get mod
	mov al, byte ptr [si]
	mov cl, al
	shr al, 6
	mov byte ptr [_mod], al
	
	; one case if mod == 11 we dont need diection flag
	cmp [_mod], 03h
	je handle_1000_10_mod11
	
	; handle direction flag when mod is 00, 01, 10
	push [_mod]
	
	;; if _d == 0 mem/reg -> reg so we can leave the _mod, and load the second reg/mem into al
	cmp [_d], 0
	jne handle_1000_10_skip_swap_1
	mov al, cl
	and al, 07h
	
	handle_1000_10_skip_swap_1:
		;; if _d == 1 reg -> mem/reg so we need to clear the _mod, and load the first reg/mem into al
		cmp [_d], 1
		jne handle_1000_10_skip_swap_2
		mov byte ptr [_mod], 03h
		mov al, cl
		and al, 38h
		shr al, 3
	
	handle_1000_10_skip_swap_2:
	call move_regmem_to_bx
	
	; add a comma and a white space
	COMMA_BUFFER_OUT
	WHITE_SPACE_BUFFER_OUT
	
	pop [_mod]
	
	; if _d == 1 reg -> mem/reg so we can leave the _mod, and load the second reg/mem into al
	cmp [_d], 1
	jne handle_1000_10_skip_swap_3
	mov al, cl
	and al, 07h
	
	handle_1000_10_skip_swap_3:
		;; if _d == 0 mem/reg -> reg so we need to clear the _mod, and load the first reg/mem into al
		cmp [_d], 0
		jne handle_1000_10_skip_swap_4
		mov byte ptr [_mod], 03h
		mov al, cl
		and al, 38h
		shr al, 3

	handle_1000_10_skip_swap_4 :
	call move_regmem_to_bx
	
	NEW_LINE_BUFFER_OUT
	call handle_buffer_out
	
	pop dx cx
	ret
	
	;; handle mod11 since it doesnt need direction flag unlike mod00,01,10 (SUCH A SNOWFLAKE OH MY GOUDDD)
	handle_1000_10_mod11:
		; extract the first reg/mem
		mov al, cl
		and al, 38h
		shr al, 3
		
		;; print the corespoding reg
		call move_regmem_to_bx
		
		; add a comma and a white space
		COMMA_BUFFER_OUT
		WHITE_SPACE_BUFFER_OUT
		
		;; extract the second byte
		mov al, cl
		and al, 07h
		;; print the second reg/mem
		
		call move_regmem_to_bx
		;; check for offsetn
		
		NEW_LINE_BUFFER_OUT
		call handle_buffer_out
		
		pop dx cx
	ret
endp

; expects to al to be the reg and requires w, mod and bx to be pointer to buffer_out to be in bx
move_regmem_to_bx proc
	push cx
	cmp  [_mod], 00
	je _mod_00_l
	
	cmp byte ptr [_mod], 03h
	je _mod_11_l
	
	call mov_mod0110_reg_to_bx
	
	;; call mod0110
	jmp _move_regmem_to_bx_exit
	
	_mod_00_l:
	call mov_mod00_reg_to_bx
	jmp _move_regmem_to_bx_exit
	
	_mod_11_l:
	call mov_mod11_reg_to_bx
	jmp _move_regmem_to_bx_exit
	
	_move_regmem_to_bx_exit:
	pop cx
	ret 
endp

; expects bx to be the first byte to write to, al to be the registers code
mov_mod11_reg_to_bx proc 
	; if w == 0 we need to load di with byte registers else load with word registers
	cmp [_w], 0				
	je _mov_mod11_reg_to_bx_w0
	
	lea di, opcode_table_std_wreg_nr
	jmp _mov_mod11_reg_to_bx_look_up
	
	_mov_mod11_reg_to_bx_w0:
	lea di, opcode_table_std_breg_nr
	
	; expects di to point to our opcode table
	_mov_mod11_reg_to_bx_look_up:
		mov cl, byte ptr [di]
		and cl, 07h
		cmp al, cl
		je _load_op_mov_mod11_reg_to_bx
		
		add di, OPC_REG_NAME_LEN
		jmp _mov_mod11_reg_to_bx_look_up
		
	_load_op_mov_mod11_reg_to_bx:
	
	xor ch, ch
	mov cl, OPC_REG_NAME_LEN - 1						; -1 because first byte is the code so we need to copy one byte less					
	
	push ds es cx di si
		inc di											; move to the beginning of the opcode
		mov ax, ds										; es == ds
		mov es, ax

		mov ax, di
		mov di, bx
		mov si, ax
		
		rep movsb
		
	pop si di cx es ds
	
	; add to bx the ammount we moved
	add bx, cx
	add [buffer_out_size], cl
	
	ret
endp

; expects bx to be the first byte to write to, al to be the registers code
mov_mod00_reg_to_bx proc 
	lea di, opcode_table_reg_mod00
	
	; expects di to point to our opcode table
	_mov_mod00_reg_to_bx_look_up:
		cmp al, byte ptr [di]
		je _load_op_mov_mod00_reg_to_bx
		call move_di_scnd_byte
		jmp _mov_mod00_reg_to_bx_look_up
		
	_load_op_mov_mod00_reg_to_bx:
	;; check if we had a direct adress
	cmp byte ptr [di], 06h
	je mov_mod00_reg_to_bx_dir_adrs
	
	call move_di_to_bx_scnd_byte
	
	SQRBR_R_BUFFER_OUT
	
	ret
	
	mov_mod00_reg_to_bx_dir_adrs:
	push ax										; preserve the current byte
	SQRBR_L_BUFFER_OUT							; add a square bracket [
	
	call handle_buffer_in
	mov al,  byte ptr [si]
	call mov_byte_hex_buffer_out
	
	call handle_buffer_in
	mov al,  byte ptr [si]
	call mov_byte_hex_buffer_out
	
	call swap_last_4_packs_2					; handle endian
	
	SQRBR_R_BUFFER_OUT
	
	pop ax
	ret
	
endp
	
; expects bx to be the first byte to write to, al to be the registers code
mov_mod0110_reg_to_bx proc 
	lea di, opcode_table_reg_mod00
	
	; expects di to point to our opcode table
	_mov_mod0110_reg_to_bx_look_up:
		cmp al, byte ptr [di]
		je _load_op_mov_mod0110_reg_to_bx
		call move_di_scnd_byte
		jmp _mov_mod0110_reg_to_bx_look_up
		
	_load_op_mov_mod0110_reg_to_bx:
	;; check if we had a direct adress
	cmp byte ptr [di], 06h
	jne mov_mod0110_reg_to_bx_bp_skip
	
	lea di,	_bp_
	
	mov_mod0110_reg_to_bx_bp_skip:
	call move_di_to_bx_scnd_byte
	
	WHITE_SPACE_BUFFER_OUT
	PLUS_BUFFER_OUT
	WHITE_SPACE_BUFFER_OUT
	
	push ax										; preserve the current byte
	
	; if we need only one byte skip reading of one of the bytes
	cmp [_mod], 01h
	je	mov_mod0110_reg_to_bx_skip_byte
	
	mov di, 1									; set di to 1 as a flag to know if we need to swap bytes
	
	call handle_buffer_in
	mov al,  byte ptr [si]
	call mov_byte_hex_buffer_out
	
	mov_mod0110_reg_to_bx_skip_byte:
	call handle_buffer_in
	mov al,  byte ptr [si]
	call mov_byte_hex_buffer_out
	
	cmp di, 1
	jne mov_mod0110_reg_to_bx_swap_skip
	
	call swap_last_4_packs_2					; handle endian
	
	mov_mod0110_reg_to_bx_swap_skip:
	
	SQRBR_R_BUFFER_OUT
	
	pop ax
	ret
	
endp

; assumes the byte is in al
handle_1010 proc
	push dx
	mov dl, al					; save ax for now
	
	; load the adress
	lea bx, buffer_out
	
	push ax dx
	; mov current address into buffer out
	mov ax, [current_address]
	call mov_word_hex_buffer_out
	COLON_BUFFER_OUT
	WHITE_SPACE_BUFFER_OUT
	
	pop dx ax
	; extract [_w]
	and dl, 01h
	mov [_w], dl
	
	; extract imaginary [_d]
	mov dl, al 
	and dl, 02h
	shr dl, 1
	mov [_d], dl
		
	mov dl, al
	and dl, 0Fh
	shr dl, 2
	
	cmp dl, 00h
	je handle_1010_00_l

	handle_1010_00_l:
	call handle_1010_00

	pop dx
	ret
endp

; move ax <-> mem, assumes byte -> al, and bx -> buffer out, si -> buffer_in
handle_1010_00 proc
	push cx dx

	push di
	lea di, mov_str
	mov cx, mov_str_len
	call move_command_to_bffr
	pop di
	
	;; handle if [_d] == 0, mov ax, mem
	cmp [_d], 00h
	jne handle_1010_00_d1
	
	; check if al or ax was used
	cmp [_w], 00h
	jne handle_1010_00_d0w1
	
		; move 'al' to buffer_out
		lea di, opcode_table_std_breg_nr
		jmp handle_1010_00_mov_reg_1
		
	handle_1010_00_d0w1:
		; move 'ax' to buffer_out
		lea di, opcode_table_std_wreg_nr
	
	; move the register
	handle_1010_00_mov_reg_1:
	mov cx, OPC_REG_NAME_LEN - 1
	call move_cxdi_to_bx
	
	
	; add a white space and a comma
	COMMA_BUFFER_OUT
	WHITE_SPACE_BUFFER_OUT
	
	; read the next two bytes and move them to buffer_out
	SQRBR_L_BUFFER_OUT
	
	call handle_buffer_in	
	mov al, byte ptr [si]
	
	call handle_buffer_in	
	mov ah, byte ptr [si]
	call mov_word_hex_buffer_out
	
	SQRBR_R_BUFFER_OUT
	
	jmp handle_1010_00_exit
	
	;; handle if [_d] == 1 mov mem, ax 
	handle_1010_00_d1:
		
		SQRBR_L_BUFFER_OUT
		; read the next two bytes and move them to buffer_out
		call handle_buffer_in	
		mov al, byte ptr [si]
		
		call handle_buffer_in	
		mov ah, byte ptr [si]
		call mov_word_hex_buffer_out
		
		SQRBR_R_BUFFER_OUT
		
		; add a white space and a comma
		COMMA_BUFFER_OUT
		WHITE_SPACE_BUFFER_OUT
		
		cmp [_w], 00h
		jne handle_1010_00_d1w1
	
		; move 'al' to buffer_out
		lea di, opcode_table_std_breg_nr
		jmp handle_1010_00_mov_reg_2
		
		; move 'ax' to buffer_out
		handle_1010_00_d1w1:
			lea di, opcode_table_std_wreg_nr
	
		handle_1010_00_mov_reg_2:
		; move the register
		mov cx, OPC_REG_NAME_LEN - 1
		call move_cxdi_to_bx


	handle_1010_00_exit:
	
	NEW_LINE_BUFFER_OUT
	
	call handle_buffer_out
	pop dx cx
	ret
endp

; expects the byte in al
handle_1100 proc
	mov dl, al					; save ax for now
	
	; load the adress
	lea bx, buffer_out
	
	push ax dx
	; mov current address into buffer out
	mov ax, [current_address]
	call mov_word_hex_buffer_out
	COLON_BUFFER_OUT
	WHITE_SPACE_BUFFER_OUT
	pop dx ax
	
	mov dl, al
	and dl, 0Eh
	cmp dl, 06h
	je handle_1100_011_l
	
	; handle full xxxx bytes
	mov dl, al
	and dl, 0Fh
	cmp dl, 0Dh
	je handle_1100_1101_l
	
	; commands were not found
	call handle_unknown
	jmp handle_1100_ret
	
	handle_1100_011_l:
	call handle_1100_011
	jmp handle_1100_ret
	
	handle_1100_1101_l:
	call handle_1100_1101
	jmp handle_1100_ret
	
	handle_1100_ret:
	ret
endp

handle_1100_1101 proc
	lea di, int_str
	mov cx, int_str_len
	call move_command_to_bffr
	
	call handle_buffer_in
	mov al, byte ptr [si]
	call mov_byte_hex_buffer_out
	
	NEW_LINE_BUFFER_OUT
	call handle_buffer_out
	ret	
endp

handle_1100_011 proc
	; add 'mov' to the buffer
	push di
	lea di, mov_str
	mov cx, mov_str_len
	call move_command_to_bffr
	pop di

	; extract [_w]
	mov dl, al
	and dl, 01h
	mov [_w], dl
	
	call handle_buffer_in 												; move to the next byte
	mov al, byte ptr [si]
	
	; extract [_mod]
	mov dl, al
	shr dl, 6
	mov byte ptr [_mod], dl
	
	; extract the r/m to al
	and al, 07h
	
	; write r/m to buffer_out
	call move_regmem_to_bx
	
	COMMA_BUFFER_OUT
	WHITE_SPACE_BUFFER_OUT
	
	call handle_bojb_bovb
	
	handle_1100_011_exit:
	NEW_LINE_BUFFER_OUT	
	call handle_buffer_out
	ret
endp

; needs [_w] to be set si -> buffer_in, bx -> buffer_out
handle_bojb_bovb proc
	cmp [_w], 0
	je handle_bojb_bovb_skip
	
	call handle_buffer_in
	mov al, byte ptr [si]
	call mov_byte_hex_buffer_out
	
	handle_bojb_bovb_skip:
	call handle_buffer_in
	mov al, byte ptr [si]
	call mov_byte_hex_buffer_out
	
	cmp [_w], 1
	jne handle_bojb_bovb_exit

	call swap_last_4_packs_2
	
	handle_bojb_bovb_exit:
	ret
endp

; expects bx -> buffer_out, di -> opc 'reg' on return cx -> bytes copied
move_cxdi_to_bx proc 
	;xor ch, ch
	;mov cl, OPC_REG_NAME_LEN - 1	

	push ds es cx di si
	inc di										; move to the beginning of the opcode
	mov ax, ds									; es == ds
	mov es, ax

	mov ax, di
	mov di, bx
	mov si, ax
	
	rep movsb
		
	pop si di cx es ds
	
	add bx, cx
	add [buffer_out_size], cl
	ret
endp 

handle_unknown proc
	push cx dx
	lea di, unknown_str
	mov cx, unknown_str_len
	call move_command_to_bffr
	NEW_LINE_BUFFER_OUT
	call handle_buffer_out
	pop dx cx
	ret
endp

; assumes byte is in al
handle_1111 proc
	; load the adress
	lea bx, buffer_out
	
	; mov current address into buffer out
	push ax
	mov ax, [current_address]
	call mov_word_hex_buffer_out
	COLON_BUFFER_OUT
	WHITE_SPACE_BUFFER_OUT
	pop ax
	
	; extract [_w] will be usefull for half of the family functions
	mov dl, al
	and dl, 01h
	mov [_w], dl
	
	; check if the command id was 011
	mov dl, al
	and dl, 0Eh
	shr dl, 1

	cmp dl, 03h
	je _handle_1111_011x_l
	
	cmp dl, 07h
	je _handle_1111_111x_l
	
	call handle_unknown
	jmp _handle_1111_exit
	
	_handle_1111_011x_l:
	call handle_1111_011x
	jmp _handle_1111_exit
	
	_handle_1111_111x_l:
	call handle_1111_111x
	jmp _handle_1111_exit
	
	_handle_1111_exit:
	
	ret
endp

; assumes the byte is in al, bx -> buffer_out, _w is set
handle_1111_011x proc
	call handle_buffer_in
	mov al, byte ptr [si]

	; extract mod
	mov dl, al
	shr dl, 6
	mov byte ptr [_mod], dl

	; extract the look up bits and place them in al
	mov dl, al
	and dl, 38h
	shr dl, 3
	mov al, dl
	
	lea di, opcode_1111_011
	push cx
	mov cx, OPC_1111_011_COUNT
	handle_1111_011x_look_up:
		cmp byte ptr [di], al
		je handle_1111_011x_opc_found
		
		call move_di_scnd_byte
		loop handle_1111_011x_look_up
	
	handle_1111_011x_opc_found:
	pop cx
	; move the current operation name to buffer_out
	call move_di_to_bx_scnd_byte
	
	; add a white space
	WHITE_SPACE_BUFFER_OUT
	
	; extract r/m
	mov al, byte ptr [si]
	and al, 07h
	
	; move handle r/m
	call move_regmem_to_bx
		
	NEW_LINE_BUFFER_OUT
	call handle_buffer_out
	ret
endp

; assumes the byte is in al, bx 
handle_1111_111x proc
	call handle_buffer_in
	mov al, byte ptr [si]
	
	; extract mod
	mov dl, al
	shr dl, 6
	mov byte ptr [_mod], dl
	
	; look up bits
	mov dl, al
	and dl, 38h
	shr dl, 3
	
	; load the look up table to di
	lea di, opcode_1111_111
	
	push cx
	mov cx, OPC_1111_111_COUNT

	; start the search
	_handle_1111_111x_look_up:
		cmp dl, byte ptr [di]
		je _handle_1111_111x_opc_found
		
		call move_di_scnd_byte
	
		loop _handle_1111_111x_look_up
		
	; add the command name to buffer_out
	_handle_1111_111x_opc_found:
	pop cx
	call move_di_to_bx_scnd_byte
	
	WHITE_SPACE_BUFFER_OUT
	
	; extract r/m
	mov al, byte ptr [si]
	and al, 07h
	
	; move handle r/m
	call move_regmem_to_bx
	
	NEW_LINE_BUFFER_OUT
	call handle_buffer_out
	ret
endp 

; assumes the byte is in al
handle_0100 proc
	; print the current adress
	lea bx, buffer_out
	
	; mov current address into buffer out
	push ax
	mov ax, [current_address]
	call mov_word_hex_buffer_out
	COLON_BUFFER_OUT
	WHITE_SPACE_BUFFER_OUT
	pop ax

	; extract the look_up byte
	mov dl, al
	and dl, 08h
	shr dl, 3
	
	lea di, opcode_0100
	push cx 
	mov cx, OPC_0100_COUNT
	
	_handle_0100_look_up:
	cmp dl, byte ptr [di]
	je _handle_0100_opc_found
	
	call move_di_scnd_byte
	
	loop _handle_0100_look_up
	
	_handle_0100_opc_found:
	call move_di_to_bx_scnd_byte
	WHITE_SPACE_BUFFER_OUT
	
	pop cx
	mov [_w], 1
	
	; extract the byte
	and al, 07h
	call mov_mod11_reg_to_bx
	
	NEW_LINE_BUFFER_OUT
	call handle_buffer_out
	ret 
endp

; assumes the byte is in al
handle_0101 proc
	; print the current adress
	lea bx, buffer_out
	
	; mov current address into buffer out
	push ax
	mov ax, [current_address]
	call mov_word_hex_buffer_out
	COLON_BUFFER_OUT
	WHITE_SPACE_BUFFER_OUT
	pop ax

	; extract the look_up byte
	mov dl, al
	and dl, 08h
	shr dl, 3
	
	lea di, opcode_0101
	push cx 
	mov cx, OPC_0101_COUNT
	
	_handle_0101_look_up:
	cmp dl, byte ptr [di]
	je _handle_0101_opc_found
	
	call move_di_scnd_byte
	
	loop _handle_0101_look_up
	
	_handle_0101_opc_found:
	call move_di_to_bx_scnd_byte
	WHITE_SPACE_BUFFER_OUT
	
	pop cx
	mov [_w], 1
	
	; extract the byte
	and al, 07h
	call mov_mod11_reg_to_bx
	
	NEW_LINE_BUFFER_OUT
	call handle_buffer_out
	ret 
endp

end start
