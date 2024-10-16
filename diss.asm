; Programa: Nr. 3
; Užduoties sąlyga: dissssss
; Atliko: Tomas Baublys 

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
	
	cannot_open_file db "Error while openning source file!"
					cof_err_len equ $ - cannot_open_file
					
	cannot_create_file db "Error while creating/truncating destination file!"
					ccf_err_len equ $ - cannot_create_file
	
	eof_success db "Disassembly complete. End of file reached successfully."
		eof_success_len equ $ - eof_success

	buffer_in_size db ?
	buffer_in db BUFFER_IN_LEN dup(?)
	
	byte_buffer_size db ?
	byte_buffer db BYTE_BUFFER_LEN dup(?)
	byte_buffer_iter dw ? 
	
	current_address dw 00FFh
	
	buffer_out_size db ?
	buffer_out db BUFFER_OUT_LEN dup(?)
	
	help_msg db 'Usage: diss output_file input_file', 0Dh, 0Ah
			db 'Disassembles 16-bit 8086 machine code.', 0Dh, 0Ah
			db 'Example: diss output.asm input.com', 0Dh, 0Ah
			db ' - output_file: The file to write the disassembly to.', 0Dh, 0Ah
			db ' - input_file: The binary file to disassemble.', 0Dh, 0Ah
			db 0
	help_msg_len equ $ - help_msg
	
	_d db ?
	_w db ?
	_mod db ?										; word because we need to push / pop sometimes (OPTIMZE LATER)
	_s db ?
	_v db ?
	
	; for mov reg, imm8/imm16
	opcode_table_std_breg_nr db 00h, 'al'
					_cl db 01h, 'cl'
						db 02h, 'dl'
						db 03h, 'bl'
						db 04h, 'ah'
						db 05h, 'ch'
						db 06h, 'dh'
						db 07h, 'bh'
	opcode_table_std_wreg_nr db 08h, 'ax'
					    db 09h, 'cx'
					_dx	db 0Ah, 'dx'
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
	
	opcode_0111 db 00h, 04h, 'jo '
				db 01h, 05h, 'jno '
				db 02h, 04h, 'jc '
				db 03h, 05h, 'jae '
				db 04h, 04h, 'je '
				db 05h,	05h, 'jne '
				db 06h, 05h, 'jbe '
				db 07h, 04h, 'ja '
				db 08h, 04h, 'js '
				db 09h, 05h, 'jns '
				db 0Ah, 04h, 'jp '
				db 0Bh, 05h, 'jnp '
				db 0Ch, 04h, 'jl '
				db 0Dh, 05h, 'jge '
				db 0Eh, 05h, 'jle '
				db 0Fh, 04h, 'jg '
				
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
	
	opcode_1111_011 db 00h, 05h, 'test'
					db 02h, 04h, 'not'
					db 03h, 04h, 'neg'
					db 04h, 04h, 'mul'
					db 05h, 05h, 'imul'
					db 06h, 04h, 'div'
					db 07h, 05h, 'idiv'
					
					
	opcode_1111_111 db 00h, 04h, 'inc'
					db 01h, 04h, 'dec'
			  _call db 02h, 05h, 'call'
					db 03h, 05h, 'call'
					db 04h, 04h, 'jmp'
					db 05h, 04h, 'jmp'
			  _push db 06h, 05h, 'push'
					
	opcode_0100 db 00h, 04h, 'inc'
				db 01h, 04h, 'dec'
				
				
	opcode_0101 db 00h, 05h, 'push'
				db 01h, 04h, 'pop'
				
	opcode_1111_0000_to_0101_1000_to_1101 db 00h, 05h, 'lock'
										  db 02h, 06h, 'repnz'
										  db 03h, 04h, 'rep' 
										  db 04h, 04h, 'hlt'
										  db 05h, 04h, 'cmc'
										  db 08h, 04h, 'clc'
										  db 09h, 04h, 'stc'
										  db 0Ah, 04h, 'cli'
										  db 0Bh, 04h, 'sti'
										  db 0Ch, 04h, 'cld'
										  db 0Dh, 04h, 'std'
										  
	opcode_1001 db 00h, 04h, 'nop'
				db 08h, 04h, 'cbw'
				db 09h, 04h, 'cwd'
				db 0Ah, 05h, 'call'
				db 0Bh, 05h, 'wait'
				db 0Ch, 06h, 'pushf'
				db 0Dh, 05h, 'popf'
				db 0Eh, 05h, 'sahf'
				db 0Fh, 05h, 'lahf'
		  _xchg db 00h, 05h, 'xchg'				; if the it loops through all elements di will be placed here
		  
	opcode_1000_00 db 00h, 04h, 'add'		
			   _or db 01h, 03h, 'or'
			  _adc db 02h, 04h, 'adc'
			  _sbb db 03h, 04h, 'sbb'
			  _and db 04h, 04h, 'and'
			  _sub db 05h, 04h, 'sub'
			  _xor db 06h, 04h, 'xor'
			  _cmp db 07h, 04h, 'cmp'
				   
	opcode_1000_010x_to_111x db 02h, 05h, 'movs' ; move b or w depending to the _w
							 db 03h, 05h, 'cmps'
							 db 04h, 05h, 'test'
							 db 05h, 05h, 'stos'
							 db 06h, 05h, 'lods'
							 db 07h, 05h, 'scas'
						 
	 opcode_1000_else_w db 02h, 05h, 'test'
						db 03h, 05h, 'xchg'
	 opcode_1000_else_no_w db 0Dh, 04h, 'lea'
					       db 0Fh, 04h, 'pop'
						   
	 opcode_1101_00 db 00h, 04h, 'rol'
					db 01h, 04h, 'ror'
					db 02h, 04h, 'rcl'
					db 03h, 04h, 'rcr'
					db 04h, 04h, 'shl'
					db 05h, 04h, 'shr'
					db 07h, 04h, 'sar'
					
					
	opcode_1100_ae_1011 db 0Bh, 05h, 'retf'
						db 0Ch, 08h, 'int 03h'
						db 0Eh, 05h, 'into'
						db 0Fh, 05h, 'iret'
						
	opcode_1110_be_0011 db 00h, 07h, 'loopne'
					    db 01h, 06h, 'loope'
						db 02h, 05h, 'loop'
						db 03h, 05h, 'jcxz'
				   _jmp db 0Bh, 04h, 'jmp'
				   
	_in db 00h, 03h, 'in'
	_out db 01h, 04h, 'out'
	
	_aam db 04h, 04h, 'aam'
	_aad db 05h, 04h, 'aad'
	_xlat db 07h, 05h, 'xlat'
	_esc db 01h, 04h, 'esc'
	
	_les db 00h, 04h, 'les'
	_lds db 01h, 04h, 'lds'
					
	_ret db 03h, 04h, 'ret'
					
	_daa db 07h, 04h, 'daa'
	_das db 0Fh, 04h, 'das'
	
	_aaa db 07h, 04h, 'aaa' 
	_aas db 0Fh, 04h, 'aas'
							
	_pop db 07h, 04h, 'pop'
	
	_mov db 00h, 04h, 'mov'
 				
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
	
	; move the byte_buffer_iter to the beggining
	mov dx, offset byte_buffer
	mov ds:[byte_buffer_iter], dx 	
	
	; open file name
	lea dx,  ds:[output_file]
	mov ax, 3C00h
	mov cx, 00h
	int 21h
	jc cannot_open_dest_file_err
	mov ds:[output_file_handle], ax


	; open the file
	mov ah, 3Dh
	mov al, 00h														; read only
	lea dx, input_file
	int 21h
	jc cannot_open_src_file_err
	
	; save the file hande
	mov word ptr [input_file_handle], ax
	
	mov [buffer_in_size], 1
	call handle_buffer_in
	jmp parse_buffer
	
	read_buffer_jmp:
		call read_buffer
	
	;; make a tree later for now compare ok!!!
	parse_buffer:
		mov al, byte ptr [si]	; load the byte for comparision
		
		mov bl, al
		shr bl, 4				; check if the call was Bxh
		
		cmp bl, 00h
		je handle_0000_l
		
		; check for 000s special cases need to be handle here
		mov dl, al
		and dl, 0E6h			; 1110 0110
		cmp dl, 06h				; 000 0 0110
		je handle_0000_l
		
		cmp bl, 01h
		je handle_0001_l
		
		mov dl, al
		and dl, 0E7h
		cmp dl, 26h
		je handle_0010_l
		
		cmp bl, 02h
		je handle_0010_l
		
		cmp bl, 03h
		je handle_0011_l
		
		cmp bl, 04h
		je handle_0100_l
		
		cmp bl, 05h 
		je handle_0101_l

		cmp bl, 07h
		je handle_0111_l
		
		cmp bl, 08h
		je handle_1000_l
		
		cmp bl, 09h
		je handle_1001_l
		
		cmp bl, 0Ah
		je handle_1010_l
		
		cmp bl, 0Bh
		je	handle_1011_l	
		
		cmp bl, 0Ch
		je handle_1100_l
		
		cmp bl, 0Dh
		je handle_1101_l
		
		cmp bl, 0Eh
		je handle_1110_l
		
		cmp bl, 0Fh
		je handle_1111_l
		
		lea bx, buffer_out
		call handle_unknown
		jmp _continue_loop
		
		handle_0000_l:
		call handle_0000
		jmp _continue_loop
		
		handle_0001_l:
		call handle_0001
		jmp _continue_loop
		
		handle_0010_l:
		call handle_0010
		jmp _continue_loop
		
		handle_0011_l:
		call handle_0011
		jmp _continue_loop
		
		handle_0100_l:
		call handle_0100
		jmp _continue_loop
		
		handle_0101_l:
		call handle_0101
		jmp _continue_loop
		
		handle_0111_l:
		call handle_0111
		jmp _continue_loop
		
		handle_1000_l:
		call handle_1000
		jmp _continue_loop
		
		handle_1001_l:
		call handle_1001
		jmp _continue_loop
		
		handle_1010_l:
		call handle_1010
		jmp _continue_loop
		
		handle_1011_l:
		call handle_1011
		jmp _continue_loop
		
		handle_1100_l:
		call handle_1100
		jmp _continue_loop
		
		handle_1101_l:
		call handle_1101
		jmp _continue_loop
		
		handle_1110_l:
		call handle_1110
		jmp _continue_loop
		
		handle_1111_l:
		call handle_1111
		jmp _continue_loop
		
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
	
eof_reached:		
	; get cursor position dl contains collumn
	mov ah, 03h
	mov bh, 0
	int 10h
	
	lea si, eof_success
	; add color green cuz why not
	mov cx, eof_success_len
	_eof_reached_loop:
		push cx
		lodsb							; load from si one byte to al, ++si
		mov cx, 01h
		mov bl, 0Ah
		mov ah, 09h
		int 10h
		
		; update the cursor
		inc dl
		mov ah, 02h
		int 10h
		
		pop cx
		loop _eof_reached_loop
	
	jmp exit
	
cannot_open_src_file_err:	
	; close output file
	mov bx, [output_file_handle]
	mov ah, 3Eh
	int 21h
	
	; get cursor position dl contains collumn
	mov ah, 03h
	mov bh, 0
	int 10h
	
	lea si, cannot_open_file
	; add color green cuz why not
	mov cx, cof_err_len
	_cannot_open_src_file_err_loop:
		push cx
		lodsb							; load from si one byte to al, ++si
		mov cx, 01h
		mov bl, 0Ch						; color - red or use 04h
		mov ah, 09h
		int 10h
		
		; update the cursor
		inc dl
		mov ah, 02h
		int 10h
		
		pop cx
		loop _cannot_open_src_file_err_loop
	
	jmp exit_to_dos	
	
cannot_open_dest_file_err:						; cannot open file
	; get cursor position dl contains collumn
	mov ah, 03h
	mov bh, 0
	int 10h
	
	lea si, cannot_create_file
	; add color green cuz why not
	mov cx, ccf_err_len
	_cannot_open_dest_file_err_loop:
		push cx
		lodsb							; load from si one byte to al, ++si
		mov cx, 01h
		mov bl, 0Ch						; color - red or use 04h
		mov ah, 09h
		int 10h
		
		; update the cursor
		inc dl
		mov ah, 02h
		int 10h
		
		pop cx
		loop _cannot_open_dest_file_err_loop
	
	jmp exit_to_dos	
	
exit:
	; close output file
	mov bx, [output_file_handle]
	mov ah, 3Eh
	int 21h
	
	; close input file
	mov bx, [input_file_handle]
	mov ah, 3Eh
	int 21h

exit_to_dos:
    ; Exit to DOS
    mov ax, 4C00h
    int 21h
	
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
	push	ax cx
	call	skip_spaces
	mov cx, 0
	read_filename_start:
		cmp	byte ptr ds:[si], 13		; compare with a new line
		je	read_filename_end			; if yes we are done
		
		inc cx
		
		cmp cx, 10h						; user played too much 
		ja exit_to_dos
		
		cmp	byte ptr ds:[si], ' '		; if not white spaces, continue reading
		jne	read_filename_next			
	read_filename_end:
		mov	al, 0						; write 0 to the end
		stosb                           ; Store AL at address ES:(E)DI, di = di + 1
		pop	cx ax
		ret
	read_filename_next:
		lodsb							; mov ds:si -> al
		stosb                           ; Store AL at address ES:(E)DI, di = di + 1
		jmp read_filename_start
endp

get_mod proc
	push ax
	shr al, 6
	mov byte ptr [_mod], al
	pop ax
	ret
endp

get_d proc
	push ax
	and al, 02h
	shr al, 1
	mov byte ptr [_d], al
	pop ax
	ret
endp

get_v proc
	push ax
	and al, 02h
	shr al, 1
	mov byte ptr [_v], al
	pop ax
	ret
endp

get_w proc
	push ax
	and al, 01h
	mov byte ptr [_w], al
	pop ax
	ret
endp

add_h proc
	mov byte ptr [bx], 'h'
	inc bx
	inc [buffer_out_size]
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

; expects bx -> buffer_out, di -> opc 'reg' on return cx -> bytes copied
move_cxdi_to_bx proc 
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
	push ds es ax si cx
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
		
	pop cx si ax es ds
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

mov_byte_hex_byte_buffer proc
	push cx ax
	
	xor ah, ah
	mov cl, 10h
	div cl
	
	push ax 				; save the remainder

	cmp al, 0Ah
	jb _mov_byte_hex_byte_buffer_first_hex
	
	add al, 7				; add if its a letter
	
	_mov_byte_hex_byte_buffer_first_hex:
		add al, '0'
		mov byte ptr [bx], al
		inc bx
		inc [byte_buffer_size]
	
	pop ax
	
	mov al, ah 
	cmp al, 0Ah
	jb _mov_byte_hex_byte_buffer_second_hex
	
	add al, 7
	
	_mov_byte_hex_byte_buffer_second_hex:
		add al, '0'
		mov byte ptr [bx], al
		inc bx
		inc [byte_buffer_size]
		
	add byte_buffer_iter, 2

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

handle_xjb_xvb proc
	call handle_buffer_in
	mov al, byte ptr [si]
	call mov_byte_hex_buffer_out
	
	call handle_buffer_in
	mov ah, byte ptr [si]
	call mov_word_hex_buffer_out
	
	call add_h
	
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
	call add_h
	
	ret
endp

move_curr_address_buffer_out proc
	push ax dx
	mov ax, [current_address]
	call mov_word_hex_buffer_out
	mov byte ptr [bx], 'h'
	inc bx
	inc [buffer_out_size]
	
	COLON_BUFFER_OUT
	WHITE_SPACE_BUFFER_OUT
	pop dx ax
	
	ret
endp	

; assumes byte is loaded to al _d, _w are set, moves to the needed byte itself
handle_dw_mod_reg_rm_offset proc 
	call handle_buffer_in
	mov al, byte ptr [si]
	mov cl, al
	
	call get_mod

	cmp byte ptr [_d], 0
	je _handle_dw_mod_reg_rm_offset_d0
	
	; extract reg and move it to buffer out
	and al, 38h
	shr al, 03h
	call mov_mod11_reg_to_bx

	COMMA_BUFFER_OUT
	WHITE_SPACE_BUFFER_OUT
	
	; extract the second byte and move it to buffer out
	mov al, byte ptr [si]									; move from here since previuos called mod11 modifies cx
	and al, 07h
	call move_regmem_to_bx
	jmp handle_dw_mod_reg_rm_offset_ret
	
	_handle_dw_mod_reg_rm_offset_d0:		
		; extract rm and move it to buffer out
		and al, 07h
		call move_regmem_to_bx

		COMMA_BUFFER_OUT
		WHITE_SPACE_BUFFER_OUT
		
		; extract the reg byte and move it to buffer out
		mov al, cl
		and al, 38h
		shr al, 3
		call mov_mod11_reg_to_bx

	handle_dw_mod_reg_rm_offset_ret:
	ret
endp 	

handle_unknown proc
	push cx dx
	lea di, unknown_str
	mov cx, unknown_str_len
	call move_command_to_bffr
	call handle_buffer_out
	pop  dx cx
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
	
	call handle_buffer_in
	mov ah,  byte ptr [si]
	call mov_word_hex_buffer_out
	
	mov byte ptr [bx], 'h'
	inc bx
	inc [buffer_out_size]
	
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
	mov byte ptr [bx], 'h'
	inc bx
	inc [buffer_out_size]
	
	SQRBR_R_BUFFER_OUT
	
	pop ax
	ret
	
endp
	
; assumes the byte is in al
handle_0000 proc
	push cx
	
	lea bx, buffer_out

	; mov current address into buffer out
	call move_curr_address_buffer_out
	
	; check if it was either r110 or r111
	mov dl, al
	and dl, 07h
	cmp dl, [_pop]
	je _handle_0000_pop 

	cmp dl, [_push]
	je _handle_0000_push
	
	; handle others		
	; get _w
	call get_w

	; check if xydw or xy0w 
	mov dl, al
	
	and dl, 04h
	cmp dl, 04h
	je _handle_0000_xx0w

	; else handle the last case
	; load _d
	call get_d
	
	; move the apropriate command to buffer out
	and al, 08h
	shr al, 03h
	
	cmp al, byte ptr [_or]
	je _handle_0000_else_or
	
	lea di, opcode_1000_00
	jmp _handle_0000_else_or_add
	
	_handle_0000_else_or:
		lea di, _or
		
	_handle_0000_else_or_add:
	call move_di_to_bx_scnd_byte
	
	WHITE_SPACE_BUFFER_OUT
	
	call handle_dw_mod_reg_rm_offset
	
	; exit
	_handle_0000_exit:
	call handle_buffer_out
	pop cx
	ret
	
	_handle_0000_pop:
		lea di, _pop 
		jmp _handle_0000_pop_push
		
	_handle_0000_push:
		lea di, _push 
	
	_handle_0000_pop_push:
		call move_di_to_bx_scnd_byte
		
		WHITE_SPACE_BUFFER_OUT
		
		; extract sr
		and al, 18h
		shr al, 3
		call mov_sr_to_bx
		jmp _handle_0000_exit
		
	_handle_0000_xx0w:
		mov dl, al
		and dl, 08h
		shr dl, 3
		cmp dl, byte ptr [_or]
		je _handle_0000_xx0w_or
		
		; add is the first in the table
		lea di, opcode_1000_00
		jmp _handle_0000_xx0w_or_add
		
		_handle_0000_xx0w_or:
		lea di, _or

		_handle_0000_xx0w_or_add:
		call move_di_to_bx_scnd_byte
		
		WHITE_SPACE_BUFFER_OUT
		
		; move acumulator to buffer_out
		mov ax, 0000h
		call mov_mod11_reg_to_bx
		
		COMMA_BUFFER_OUT
		WHITE_SPACE_BUFFER_OUT
		
		call handle_bojb_bovb
		
		jmp _handle_0000_exit
	
endp	
	
; assumes the byte is in al
handle_0001 proc
	lea bx, buffer_out
	; move address to buffer_out
	call move_curr_address_buffer_out
	
	; read _w, _d
	call get_w
	call get_d
	
	; extract first deciding bit
	mov dl, al
	and dl, 08h
	shr dl, 03h
	
	; move the commad to buffer out depending on the byte  0 == adc, 1 == sbb
	cmp dl, 01h
	je _handle_0001_sbb

		lea di, _adc
	jmp _handle_0001_after_command
	
	_handle_0001_sbb:
		lea di, _sbb
		
	_handle_0001_after_command:
	; move the command to buffer out
	call move_di_to_bx_scnd_byte
	WHITE_SPACE_BUFFER_OUT
	
	; extract the second deciding byte
	mov dl, al
	and dl, 04h
	shr dl, 02h
	
	; dl == 0 dw_mod_reg_offset, dl == 1 bojb [bovb]
	cmp dl, 01h
	je _handle_0001_bojb_bovb
	
	call handle_dw_mod_reg_rm_offset
	
	jmp _handle_0001_ret
	_handle_0001_bojb_bovb:
		mov al, 00h
		call mov_mod11_reg_to_bx
		
		COMMA_BUFFER_OUT
		WHITE_SPACE_BUFFER_OUT
		
		call handle_bojb_bovb 

	_handle_0001_ret:
	call handle_buffer_out
	ret
endp
	
; assumes the byte is in al
handle_0010 proc
	lea bx, buffer_out
	call move_curr_address_buffer_out
	
	; check if it was daa or das
	mov dl, al
	and dl, 0Fh
	
	cmp dl, [_daa]
	je _handle_0010_daa
	
	cmp dl, [_das]
	je _handle_0010_das
	
	; check if it was 
	and dl, 07h
	cmp dl, 06h
	je _handle_0010_sgmch
	; handle everything else
	
	; get _w
	call get_w
	; extract the first 4 bit
	mov dl, al
	and dl, 08h
	shr dl, 03h
	cmp dl, 01h					; dl == 1 -> sub, dl == 0 -> and
	je _handle_0010_sub
	
	lea di, _and
	jmp _handle_0010_sub_and
	
	_handle_0010_sub:
		lea di, _sub
	
	_handle_0010_sub_and:
	; move the command to buffer_out
	call move_di_to_bx_scnd_byte
	WHITE_SPACE_BUFFER_OUT
	
	; now extract second deciding byte
	mov dl, al
	and dl, 04h
	shr dl, 02h
	cmp dl, 00h					; dl == 0 -> dw_mod_reg_r/m_offset dl == 1 -> bojb bovb
	je _handle_0010_dw_mod_reg_rm_offset

	mov al, 00h
	call mov_mod11_reg_to_bx
	COMMA_BUFFER_OUT
	WHITE_SPACE_BUFFER_OUT
	
	call handle_bojb_bovb
	jmp _handle_0010_ret

	_handle_0010_dw_mod_reg_rm_offset:
	call get_d
	call handle_dw_mod_reg_rm_offset

	
	_handle_0010_ret:
	call handle_buffer_out
	ret 
	
	_handle_0010_daa:
	lea di, _daa
	call move_di_to_bx_scnd_byte
	jmp _handle_0010_ret
	
	_handle_0010_das:
	lea di, _das
	call move_di_to_bx_scnd_byte
	jmp _handle_0010_ret
	
	_handle_0010_sgmch:
	and al, 018h
	shr al, 03h
	call mov_sr_to_bx
	mov byte ptr [bx], ':'
	inc bx
	inc [buffer_out_size]
	jmp _handle_0010_ret
	
endp
	
; assumes the byte is in al
handle_0011 proc
	lea bx, buffer_out
	call move_curr_address_buffer_out
	
	; check if it was daa or das
	mov dl, al
	and dl, 0Fh
	
	cmp dl, [_aaa]
	je _handle_0011_aaa
	
	cmp dl, [_aas]
	je _handle_0011_aas ;;
	; handle everything else
	
	; get _w
	call get_w
	; extract the first 4 bit
	mov dl, al
	and dl, 08h
	shr dl, 03h
	cmp dl, 01h					; dl == 1 -> cmp, dl == 0 -> xor
	je _handle_0011_cmp
	
	lea di, _xor
	jmp _handle_0011_xor_cmp
	
	_handle_0011_cmp:
		lea di, _cmp
	
	_handle_0011_xor_cmp:
	; move the command to buffer_out
	call move_di_to_bx_scnd_byte
	WHITE_SPACE_BUFFER_OUT
	
	; now extract second deciding byte
	mov dl, al
	and dl, 04h
	shr dl, 02h
	cmp dl, 00h					; dl == 0 -> dw_mod_reg_r/m_offset dl == 1 -> bojb bovb
	je _handle_0011_dw_mod_reg_rm_offset

	mov al, 00h
	call mov_mod11_reg_to_bx
	COMMA_BUFFER_OUT
	WHITE_SPACE_BUFFER_OUT
	
	call handle_bojb_bovb
	jmp _handle_0011_ret

	_handle_0011_dw_mod_reg_rm_offset:
	call get_d
	call handle_dw_mod_reg_rm_offset

	
	_handle_0011_ret:
	call handle_buffer_out
	ret 
	
	_handle_0011_aaa:
	lea di, _aaa
	call move_di_to_bx_scnd_byte
	jmp _handle_0011_ret
	
	_handle_0011_aas:
	lea di, _aas
	call move_di_to_bx_scnd_byte
	jmp _handle_0011_ret
	
endp	

; assumes the byte is in al
handle_0100 proc
	; print the current adress
	lea bx, buffer_out
	
	; mov current address into buffer out
	call move_curr_address_buffer_out

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
	
	call handle_buffer_out
	ret 
endp

; assumes the byte is in al
handle_0101 proc
	; print the current adress
	lea bx, buffer_out
	
	; mov current address into buffer out
    call move_curr_address_buffer_out

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

	call handle_buffer_out
	ret 
endp	

; expects current byte in al THIS NEEDS TO BE REDONE (brought up ot framework levels)
handle_0111 proc
	push cx ax bx
	
	lea bx, buffer_out
	call move_curr_address_buffer_out
	
	; find the coresponding conditional jump
	and al, 0Fh
	
	lea di, opcode_0111
	mov cx, OPC_0111_COUNT
	_handle_0111_look_up:
		cmp byte ptr [di], al
		je _handle_0111_continue
		
		call move_di_scnd_byte
		
		jmp _handle_0111_look_up
	
	; opcode found
	_handle_0111_continue:
	call move_di_to_bx_scnd_byte					; copy conditional jump call to buffer
	
	;call handle_buffer_in							; check if we ran out of buffer_in
	mov [_w], 0
	call handle_bojb_bovb
	
	call handle_buffer_out							; flush the buffer to stdout
	
	pop bx ax cx
	ret
endp	
	
; assumes the byte is in al
handle_1000 proc
	push dx cx
	
	lea bx, buffer_out
	
	; mov current address into buffer out
	call move_curr_address_buffer_out
	
	; check if the call was 1000_10dw
	mov dl, al
	and dl, 0Ch
	cmp dl, 08h
	je _handle_1000_10_l
	
	; check if the call was 1000_00sw
	cmp dl, 00h
	je _handle_1000_00xx_l
	
	;; check if the call was 1000_11x0
	mov dl, al
	and dl, 0Dh
	cmp dl, 0Ch
	je _handle_1000_11x0_l
	
	call handle_1000_else
	jmp _handle_1000_ret
	
	call handle_unknown
	jmp _handle_1000_ret
	
	_handle_1000_00xx_l:
	call handle_1000_00
	jmp _handle_1000_ret
	
	_handle_1000_10_l:
	call handle_1000_10
	jmp _handle_1000_ret
	
	_handle_1000_11x0_l:
	call handle_1000_11x0
	jmp _handle_1000_ret
	
	_handle_1000_ret:
	
	pop cx dx
	ret
endp 

; assumes byte is in al
handle_1000_00 proc
	; extract _s
	mov dl, al
	and dl, 02h
	shr dl, 1
	mov byte ptr [_s], dl
		
	; extract _w
	and al, 01h
	mov byte ptr [_w], al
	
	; move to the next byte
	call handle_buffer_in
	mov al, byte ptr [si]
	
	; extract _mod
	mov dl, al
	shr dl, 6
	mov byte ptr [_mod], dl

	; find the corresponding commad
	lea di, opcode_1000_00
	
	and al, 38h
	shr al, 03h
	
	push cx
	mov cx, OPC_1000_00_COUNT
	_handle_1000_00_look_up:
		cmp byte ptr [di], al
		je _handle_1000_00_opc_found
		
		call move_di_scnd_byte
	
		loop _handle_1000_00_look_up
		
	; opcode found, move the command to buffer_out
	_handle_1000_00_opc_found:
	pop cx
	call move_di_to_bx_scnd_byte
	
	WHITE_SPACE_BUFFER_OUT
	
	; extract rm and move it to buffer_out
	mov al, byte ptr [si]
	and al, 07h
	call move_regmem_to_bx
	COMMA_BUFFER_OUT
	WHITE_SPACE_BUFFER_OUT
	
	; if _s = 0 and w = 1 or 0 default handling
	cmp [_s], 00h
	je _handle_1000_00_s0
	
	; if w = 0 still can be handled in the regular handle_bojb_bovb procedure
	cmp [_w], 00h
	je _handle_1000_00_s0

	; else read on byte and extract it (the byte is currently in al)
	call handle_buffer_in
	mov al, byte ptr [si]
	
	xor ah, ah
	cmp ax, 0080h											; use unsigned comparision to check if the msb in al is 1 if so fill ah with 1
	jb _handle_1000_00_move_bytes							; if above or equal, fill the bytes with 1111
	
	mov ah, -1
	
	_handle_1000_00_move_bytes:
	call mov_word_hex_buffer_out
	mov byte ptr [bx], 'h'
	inc bx
	inc [buffer_out_size]

	_handle_1000_00_exit:

	call handle_buffer_out
	ret
	
	_handle_1000_00_s0:
	call handle_bojb_bovb
	jmp _handle_1000_00_exit
endp

; assumes the byte is in al handles all 1000 family calls except 00, 01, 10
handle_1000_else proc
	push cx
	; extract _w
	call get_w
	
	call handle_buffer_in
	mov dl, al								; save al
	mov al, byte ptr [si]
	
	; extract _mod
	call get_mod
	
	; get only the id bytes (USE DL FOR LOOK UP)
	and dl, 0Fh
	cmp dl, 08h
	jae _handle_1000_else_no_w
	
	; handle only when al is xxxx 0xxxx
	mov cx, OPC_1000_ELSE_COUNT													
	lea di, opcode_1000_else_w
	shr dl, 01h
	_handle_1000_else_look_up_w:
		cmp dl, byte ptr [di]
		je _handle_1000_else_opc_found_w
		
		call move_di_scnd_byte
		loop _handle_1000_else_look_up_w
		
	_handle_1000_else_opc_found_w:
	call move_di_to_bx_scnd_byte
	
	_handle_1000_else_read_both_regs:
	WHITE_SPACE_BUFFER_OUT
	;extract the first reg
	and al, 38h
	shr al, 3
	call mov_mod11_reg_to_bx
	
	COMMA_BUFFER_OUT
	
	_handle_1000_else_read_rm:
	WHITE_SPACE_BUFFER_OUT
	;extract the second byte
	mov al, byte ptr[si]
	and al, 07h
	call move_regmem_to_bx
	
	jmp _handle_1000_else_exit
	_handle_1000_else_no_w:

	_handle_1000_else_look_up_no_w:
	lea di, opcode_1000_else_no_w
	
	cmp dl, 0Fh
	je _handle_1000_1111
	
	call move_di_to_bx_scnd_byte
	; back track sinse we just need to read both reg and mem
	jmp _handle_1000_else_read_both_regs 
	
	_handle_1000_1111:
	call move_di_scnd_byte
	
	; backtrack again
	call move_di_to_bx_scnd_byte
	jmp _handle_1000_else_read_rm
	
	_handle_1000_else_exit:
	call handle_buffer_out
	pop cx
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
	call get_d
	
	mov byte ptr [_w], 01h				; in this case we only operate with words
	
	; move to the next byte 
	call handle_buffer_in
	mov al, byte ptr [si]				; load the next byte
	
	; fill _mod
	call get_mod
	
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
	call handle_buffer_out
	pop cx
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

	; get _d
	call get_d
	
	; get _w
	call get_w
	
	; move to the next byte
	call handle_buffer_in
	mov al, byte ptr[si]
	
	; save al for later use
	mov cl, al
	
	; get mod
	call get_mod
	
	; one case if mod == 11 we dont need diection flag
	cmp [_mod], 03h
	je handle_1000_10_mod11
	
	; handle direction flag when mod is 00, 01, 10
	push word ptr [_mod]
	
	;; if _d == 0 mem/reg -> reg so we can leave the _mod, and load the second reg/mem into al
	cmp [_d], 0
	jne handle_1000_10_skip_swap_1																				
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
	
	pop word ptr [_mod]
	
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
		
		call handle_buffer_out
		
		pop dx cx
	ret
endp

; assumes the byte is in al
handle_1001 proc
	lea bx, buffer_out
	
	; mov current address into buffer out
	call move_curr_address_buffer_out
	
	and al, 0Fh
	
	push cx
	lea di, opcode_1001
	mov cx, OPC_1001_COUNT
	handle_1001_look_up:
		cmp byte ptr [di], al
		je handle_1001_opc_found
		
		call move_di_scnd_byte
		
	loop handle_1001_look_up
	handle_1001_opc_found:
	call move_di_to_bx_scnd_byte
	
	cmp byte ptr [bx - 1], 'g'
	je handle_1001_xchg
	
	; check for 1001 1010 (call)
	cmp al, 0Ah
	jne handle_1001_exit
	WHITE_SPACE_BUFFER_OUT
	
	; handle ajb avb
	call handle_xjb_xvb
	
	WHITE_SPACE_BUFFER_OUT
	; handle srjb srvb 
	call handle_xjb_xvb
	
	handle_1001_exit:
	
	pop cx
	call handle_buffer_out
	ret
	
	handle_1001_xchg:
		WHITE_SPACE_BUFFER_OUT
		mov al, byte ptr [si]
		and al, 07h
		mov [_w], 1
		
		; move the first register to buffer_out
		call mov_mod11_reg_to_bx 
		
		COMMA_BUFFER_OUT
		WHITE_SPACE_BUFFER_OUT
		
		; move ax to buffer out
		mov al, 00h
		call mov_mod11_reg_to_bx
		jmp handle_1001_exit
	ret
endp

; assumes the byte is in al, si (buffer_in pointer) gets changed around must be played with carefully!!!
handle_1011 proc
	push cx ax bx
	
	lea bx, buffer_out
	
	;call get_w
	; extract mod
	mov dl, al
	and dl, 08h
	shr dl, 03h
	mov byte ptr [_w], dl
	
	call move_curr_address_buffer_out
	
	lea di, _mov
	call move_di_to_bx_scnd_byte
	WHITE_SPACE_BUFFER_OUT
	
	;mov al, byte ptr [si]										; mask to get the registers byte
	and al, 07h
	call mov_mod11_reg_to_bx
	COMMA_BUFFER_OUT
	WHITE_SPACE_BUFFER_OUT
	
	call handle_bojb_bovb
	
	_handle_1011_flush:
	call handle_buffer_out
	pop bx ax cx
	ret
endp	

; assumes the byte is in al
handle_1010 proc
	push dx
	mov dl, al					; save ax for now
	
	; load the adress
	lea bx, buffer_out

	; mov current address into buffer out
	call move_curr_address_buffer_out
	
	; extract [_w]
	call get_w
	
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
	
	call handle_1010_010x_111x
	jmp _handle_1010_ret

	handle_1010_00_l:
	call handle_1010_00

	_handle_1010_ret:
	pop dx
	ret
endp

; expects _w to be set
handle_1010_010x_111x proc
	lea di, opcode_1000_010x_to_111x
	
	; extract the id
	mov al, byte ptr [si]
	and al, 0Fh
	shr al, 01h
	
	push cx
	mov cx, OPC_1000_010x_to_111x_COUNT
	
	_handle_1010_010x_111x_look_up:
		cmp byte ptr [di], al
		je handle_1010_010x_111x_opc_found
		
		call move_di_scnd_byte
	
	loop _handle_1010_010x_111x_look_up
	
	handle_1010_010x_111x_opc_found:
	call move_di_to_bx_scnd_byte
	
	; check if the command was test accum <-> inst
	cmp byte ptr [bx - 1], 't'
	je _handle_1010_010x_111x_test


	; else add either w or b to the buffer out
	cmp [_w], 0
	je _handle_1010_010x_111x_add_b
		mov byte ptr [bx], 'w'
		jmp _handle_1010_010x_111x_add_bw
	
	_handle_1010_010x_111x_add_b:
		mov byte ptr [bx], 'b'
		
	_handle_1010_010x_111x_add_bw:
	inc bx
	inc [buffer_out_size]
		
	
	_handle_1010_010x_111x_exit:
	call handle_buffer_out
	
	pop cx
	ret 
	
	_handle_1010_010x_111x_test:
		WHITE_SPACE_BUFFER_OUT

		xor ax, ax
		call mov_mod11_reg_to_bx
	
		COMMA_BUFFER_OUT
		WHITE_SPACE_BUFFER_OUT
	
		call handle_bojb_bovb
		jmp _handle_1010_010x_111x_exit
	
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
	call handle_buffer_out
	pop dx cx
	ret
endp

; expects the byte in al
handle_1100 proc
	; load the adress
	lea bx, buffer_out
	
	; mov current address into buffer out
	call move_curr_address_buffer_out
	
	mov dl, al
	and dl, 0Eh
	cmp dl, 06h
	je _handle_1100_011_l
	
	; handle full check if the command was int [num]
	mov dl, al
	and dl, 0Fh
	
	cmp dl, 0Dh
	je _handle_1100_1101_l
	
	; check if the command was ret
	cmp dl, 03h
	je _handle_1100_ret_l
	
	; handle both les, lds
	mov cl, al
	shr cl, 1
	and cl, 07h
	cmp cl, 02h
	je _handle_1100_010x_l
	
	; handle retf, int 3, into iret
	cmp dl, 0Bh
	jae _handle_1100_ae_1011_l
	
	; else the commands were either ret/retn bojb bovb or retf bojb bovb
	call handle_1100_x010
	jmp _handle_1100_exit
	
	; commands were not found
	call handle_unknown
	jmp _handle_1100_exit
	
	_handle_1100_011_l:
	call handle_1100_011
	jmp _handle_1100_exit
	
	_handle_1100_1101_l:
	call handle_1100_1101
	jmp _handle_1100_exit
	
	_handle_1100_ret_l:
	lea di, _ret
	call move_di_to_bx_scnd_byte
	jmp _handle_1100_exit
	
	_handle_1100_ae_1011_l:
	call handle_1100_ae_1011
	jmp _handle_1100_exit
	
	_handle_1100_010x_l:
	call handle_1100_010x
	jmp _handle_1100_exit
	
	_handle_1100_exit:
	call handle_buffer_out
	ret
endp

handle_1100_010x proc
	; set imaginary _w and _d
	mov [_d], 1
	mov [_w], 1
	
	; load the appropriate command 
	mov cl, al
	and cl, 01h
	cmp cl, 00h
	je _handle_1100_010x_les
		lea di, _lds

	jmp _handle_1100_010x_exit
	_handle_1100_010x_les:
		lea di, _les
	
	_handle_1100_010x_exit:
	call move_di_to_bx_scnd_byte
	WHITE_SPACE_BUFFER_OUT
	call handle_dw_mod_reg_rm_offset
	
	ret
endp

handle_1100_x010 proc
	; move imaginary _w
	mov [_w], 1

	; extract the deciding byte 0000 x010 x == 0 ret/retn, x == 1 retf 
	and al, 08h
	shr al, 3
	cmp al, 01h
	je _handle_1100_x010_retf

		lea di, _ret
		jmp _handle_1100_x010_exit
	
	_handle_1100_x010_retf:
		
		; retf is the first opc
		lea di, opcode_1100_ae_1011
	
	_handle_1100_x010_exit:
	call move_di_to_bx_scnd_byte
	WHITE_SPACE_BUFFER_OUT
	call handle_bojb_bovb
	ret
endp

handle_1100_ae_1011 proc
	lea di, opcode_1100_ae_1011
	mov cx, OPC_1100_AE_1011_COUNT
	_handle_1100_ae_1011_look_up:
		cmp dl, byte ptr [di]
		je _handle_1100_ae_1011_opc_found
		
		call move_di_scnd_byte
		loop _handle_1100_ae_1011_look_up
		
	_handle_1100_ae_1011_opc_found:
	call move_di_to_bx_scnd_byte
	ret
endp

handle_1100_1101 proc
	lea di, int_str
	mov cx, int_str_len
	call move_command_to_bffr
	
	call handle_buffer_in
	mov al, byte ptr [si]
	call mov_byte_hex_buffer_out
	
	mov byte ptr [bx], 'h'
	inc bx
	inc [buffer_out_size]
	
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
	call get_w
	
	call handle_buffer_in 												; move to the next byte
	mov al, byte ptr [si]

	; extract _mod
	call get_mod
	
	; extract the r/m to al
	and al, 07h
	
	; write r/m to buffer_out
	call move_regmem_to_bx
	
	COMMA_BUFFER_OUT
	WHITE_SPACE_BUFFER_OUT
	
	call handle_bojb_bovb
	
	handle_1100_011_exit:
	ret
endp

; assumes the byte is in al
handle_1101 proc
	lea bx, buffer_out
	
	; move the current address to buffer_out
	call move_curr_address_buffer_out

	; extract id for 00
	mov dl, al
	and dl, 0Ch
	shr dl, 02h
	
	cmp dl, 00h
	je _handle_1101_00
	
	; extract t from 1101 txxx to check if the code was esc
	shr dl, 1
	cmp dl, 01h
	je _handle_1101_1xxx
	
	; else compare 1101 xxtx if t == 0 its either aam or aad
	mov dl, al
	and dl, 02h
	shr dl, 1
	cmp dl, 00h
	je _handle_1101_xx0x
	
	; else it xlat, handle it here and exit
	lea di, _xlat
	call move_di_to_bx_scnd_byte
	
	_handle_1101_exit:
	call handle_buffer_out
	ret 
	
	_handle_1101_00:
	call handle_1101_00
	jmp _handle_1101_exit
	
	_handle_1101_1xxx:
	lea di, _esc
	call move_di_to_bx_scnd_byte
	WHITE_SPACE_BUFFER_OUT
	
	;extract the xxx yyy and move it to ax
	mov cl, al
	and cl, 07h											; extract xxx
	
	; move to the next byte 
	call handle_buffer_in
	mov al, byte ptr [si]
	
	; extract mod
	call get_mod

	; extract yyy
	mov dl, al
	and dl, 38h
	
	; join xxx and yyy into one
	shr dl, 03h
	xor dh, dh
	xor ch, ch
	shl cl, 03h
	
	; cl == 00xx x000, dl == 0000 0yyy
	or cl, dl
	mov al, cl
	call mov_byte_hex_buffer_out
	mov byte ptr [bx], 'h'
	inc bx
	inc [buffer_out_size]
	
	COMMA_BUFFER_OUT
	WHITE_SPACE_BUFFER_OUT
	
	; extract the r/m
	mov al, byte ptr [si]
	and al, 07h
	call move_regmem_to_bx
	
	jmp _handle_1101_exit
	
	_handle_1101_xx0x:
	call handle_1101_xx0x
	; move si by one byte
	call handle_buffer_in
	jmp _handle_1101_exit
		
endp

handle_1101_xx0x proc 
	mov dl, al
	and dl, 01h								; dl == 0 -> aam, dl == 1 -> aad
	cmp dl, 01h
	je _handle_1101_xx0x_aad
	
		lea di, _aam 
		jmp _handle_1101_xx0x_exit
		
	_handle_1101_xx0x_aad:
	lea di, _aad
	
	_handle_1101_xx0x_exit:
	call move_di_to_bx_scnd_byte
	ret 
endp

handle_1101_00 proc
	; extract _w, _v
	call get_w
	call get_v

	; move to the next byte
	call handle_buffer_in
	mov al, byte ptr [si]
	
	; extract _mod
	call get_mod
	
	; extract the look up code
	mov dl, al
	shr dl, 03h
	and dl, 07h
	
	lea di, opcode_1101_00
	mov cx, OPC_1101_00_COUNT
	_handle_1101_00_look_up:
		cmp byte ptr [di], dl
		je _handle_1101_00_opc_found
		
		call move_di_scnd_byte
		loop _handle_1101_00_look_up
	
	_handle_1101_00_opc_found:
	; move the function to buffer_out
	call move_di_to_bx_scnd_byte
	WHITE_SPACE_BUFFER_OUT

	; extract r/m
	and al, 07h
	call move_regmem_to_bx
	COMMA_BUFFER_OUT
	WHITE_SPACE_BUFFER_OUT
	
	cmp [_v], 0 							; if _v == 0, shift by one bite _v == 1 shift by al 
	je _handle_1101_00_mov_1
	
	; else move cl to buffer_out
	lea di, _cl
	mov cx, 02h
	call move_cxdi_to_bx
	jmp _handle_1101_00_ret
	
	; else move 1 to buffer_out
	_handle_1101_00_mov_1:
		mov byte ptr [bx], '0'
		inc bx
		inc [buffer_out_size]
		mov byte ptr [bx], '1'
		inc bx
		inc [buffer_out_size]
		mov byte ptr [bx], 'h'
		inc bx
		inc [buffer_out_size]
	
	_handle_1101_00_ret:
	ret
endp

; assumes the byte is in al
handle_1110 proc
	lea bx, buffer_out
	
	; load the current address
	call move_curr_address_buffer_out
	
	; check if the lower 4 bits where <= 03h or == 0Bh (jmp)
	mov dl, al
	and dl, 0Fh
	cmp dl, 03h
	jbe _handle_1110_be_0011
	
	; maybe it was jump near
	cmp dl, 0Bh
	je _handle_1110_jmp_offset_near
	
	; check if it was in/out
	and dl, 07h
	shr dl, 1
	cmp dl, 02h
	je _handle_1110_io
	
	cmp dl, 03h
	je _handle_1110_io
	
	; else its either call or jmp 
	mov cl, al							;  save al for later
	mov dl, al
	cmp dl, 08h							; dl == 08 -> call					
	je _handle_1110_call
		lea di, _jmp
		jmp _handle_1110_call_jmp
	
	_handle_1110_call:
		lea di, _call

	_handle_1110_call_jmp:
		call move_di_to_bx_scnd_byte
		WHITE_SPACE_BUFFER_OUT
	
	; either way read two bytes
	call handle_xjb_xvb
	
	; check if two more bytes need to be read
	and cl, 0003h
	cmp cl, 02h
	jne _handle_1110_exit
	
		call handle_xjb_xvb
		
	_handle_1110_exit:
		call handle_buffer_out
		ret
	
	_handle_1110_be_0011:
	call handle_1110_be_0011
	jmp _handle_1110_exit
	
	_handle_1110_jmp_offset_near:
	lea di, _jmp
	call move_di_to_bx_scnd_byte
	WHITE_SPACE_BUFFER_OUT
	; read the offset (1 byte)
	mov [_w], 0
	call handle_bojb_bovb
	jmp _handle_1110_exit
	
	_handle_1110_io:
	call handle_1110_io
	jmp _handle_1110_exit
	
endp

handle_1110_be_0011 proc
	mov cx, OPC_1110_be_0011_COUNT
	lea di, opcode_1110_be_0011
	
	handle_1110_be_0011_look_up:
		cmp byte ptr [di], dl
		je _handle_1110_be_0011_opc_found
		
		call move_di_scnd_byte
		loop handle_1110_be_0011_look_up
		
	
	_handle_1110_be_0011_opc_found:
	call move_di_to_bx_scnd_byte
	WHITE_SPACE_BUFFER_OUT
	; handle the offset byte
	mov [_w], 0
	call handle_bojb_bovb
	ret
endp

; lower portion of dl is shifted by one
handle_1110_io proc
	cmp dl, 02h 			; dl == 02h -> in
	je _handle_1110_io_in
		lea di, _out
		mov dl, 0FFh						; use as a flag to know it was out
		jmp _handle_1110_io_cmd_done
		
	_handle_1110_io_in:
		lea di, _in
	
	_handle_1110_io_cmd_done:
	call move_di_to_bx_scnd_byte
	WHITE_SPACE_BUFFER_OUT
	
	; handle out
	cmp dl, 0FFh
	je _handle_1110_io_out

	; handle in
	call get_w
	
	mov al, 00h
	call mov_mod11_reg_to_bx
	COMMA_BUFFER_OUT
	WHITE_SPACE_BUFFER_OUT
		
	; check if the port was provided
	mov dl, byte ptr [si]
	and dl, 08h
	cmp dl, 08h							; if dl == 08h port == dx
	je _handle_1110_io_in_dx
		; handle the imm8 value
		mov [_w], 0
		call handle_bojb_bovb
		jmp _handle_1110_io_exit
	
	_handle_1110_io_in_dx:
		mov [_w], 1
		mov al, [_dx]
		and al, 07h
		call mov_mod11_reg_to_bx
		
	_handle_1110_io_exit:
	ret
	
	_handle_1110_io_out:
		; check if the port was provided
		mov dl, byte ptr [si]
		push dx
		and dl, 08h
		cmp dl, 08h							; if dl == 08h port == dx
		je _handle_1110_io_out_dx
		
			; handle the imm8 value
			mov [_w], 0
			call handle_bojb_bovb
			jmp _handle_1110_io_out_ax
		
		_handle_1110_io_out_dx:
			mov [_w], 1
			mov al, [_dx]
			and al, 07h
			call mov_mod11_reg_to_bx
	
		; mov al or ax to buffer_out
		_handle_1110_io_out_ax:
		pop dx
		mov al, dl
		call get_w
		COMMA_BUFFER_OUT
		WHITE_SPACE_BUFFER_OUT
		mov al, 00h
		call mov_mod11_reg_to_bx
		jmp _handle_1110_io_exit
endp

; assumes byte is in al
handle_1111 proc
	; load the adress
	lea bx, buffer_out
	
	; mov current address into buffer out
	call move_curr_address_buffer_out
	
	; extract [_w] will be usefull for half of the family functions
	call get_w
	
	; check if the command id was 011
	mov dl, al
	and dl, 0Eh
	shr dl, 1

	cmp dl, 03h
	je _handle_1111_011x_l
	
	cmp dl, 07h
	je _handle_1111_111x_l
	
	call handle_1111_0000_to_0101_1000_to_1101
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

; assumes the byte is in al, bx -> buffer_out
handle_1111_0000_to_0101_1000_to_1101 proc
	mov al, byte ptr [si]
	and al, 0Fh

	lea di, opcode_1111_0000_to_0101_1000_to_1101
	push cx
	mov cx, OPC_1111_0000_TO_0101_1000_TO_1101_COUNT
	_handle_1111_0000_to_0101_1000_to_1101_look_up:
		cmp byte ptr [di], al
		je _handle_1111_0000_to_0101_1000_to_1101_opc_found
		
		call move_di_scnd_byte
		loop _handle_1111_0000_to_0101_1000_to_1101_look_up

	_handle_1111_0000_to_0101_1000_to_1101_opc_found:
	call move_di_to_bx_scnd_byte

	call handle_buffer_out	
		
	pop cx
	ret
endp

; assumes the byte is in al, bx -> buffer_out, _w is set
handle_1111_011x proc
	call handle_buffer_in
	mov al, byte ptr [si]

	; extract mod
	call get_mod

	; extract the look up bits and place them in al
	mov dl, al
	and dl, 38h
	shr dl, 3
	mov al, dl
	
	; save ax for later
	push ax
	
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
	
	; check if the call was TEST
	pop ax
	cmp al, 00h
	jne _handle_1111_011x_exit
	
	COMMA_BUFFER_OUT
	WHITE_SPACE_BUFFER_OUT
	
	call handle_bojb_bovb
		
	_handle_1111_011x_exit:
	call handle_buffer_out
	ret
endp

; assumes the byte is in al, bx 
handle_1111_111x proc
	call handle_buffer_in
	mov al, byte ptr [si]
	
	; extract mod
	call get_mod
	
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

	call handle_buffer_out
	ret
endp 

end start
