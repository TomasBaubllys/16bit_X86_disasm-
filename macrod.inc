MAX_ARRAY_CAPACITY = 0100h

CMD_ARG_LEN_PTR = 80h
CMD_ARG_START_PTR = 81h

MAX_FILE_NAME_LEN = 0Ch			; read at most 13 bytes

OPC_REG_NAME_LEN = 03h
OPC_REG_COUNT = 10h

;; input buffer
BUFFER_IN_LEN = 255 ;; still something wrong with com files :(

;; output buffer
BUFFER_OUT_LEN = 20h

;; standart .code start
STD_ADDRESS_START = 200h

HELP_LENGTH = 03h               ; " /?"

NEW_LINE macro
    lea dx, newln
    mov ah, 09h
    int 21h
endm

;; assumes the address is in bx, adds colon to bx and bx+=2 (address not val)
NEW_LINE_BUFFER_OUT macro
	mov byte ptr [bx], 0Dh
	inc bx
	mov byte ptr [bx], 0Ah
	inc bx
	add [buffer_out_size], 2
endm

COLON macro
    mov dl, ':'
    mov ah, 02h
    int 21h
endm

;; assumes the address is in bx, adds colon to bx and ++bx (address not val)
COLON_BUFFER_OUT macro
	mov byte ptr [bx], ':'
	inc [buffer_out_size]
	inc bx
endm

COMMA_BUFFER_OUT macro
	mov byte ptr [bx], ','
	inc [buffer_out_size]
	inc bx
endm

PLUS_BUFFER_OUT macro
	mov byte ptr [bx], '+'
	inc [buffer_out_size]
	inc bx
endm

;; assumes the address is in bx, adds white space to bx and ++bx (address not val)
WHITE_SPACE_BUFFER_OUT macro 
	mov byte ptr [bx], ' '
	inc [buffer_out_size]
	inc bx
endm

;; assumes the address is in bx, adds white space to bx and ++bx (address not val)
SQRBR_L_BUFFER_OUT macro
	mov byte ptr[bx], '['
	inc bx
	inc [buffer_out_size]
endm

SQRBR_R_BUFFER_OUT macro
	mov byte ptr[bx], ']'
	inc bx
	inc [buffer_out_size]
endm


; assumes the number is in al prints ALWAYS prints two digits (only works for <100 integers)
PRINT_DB_NUM macro
    mov cl, 10
    xor ah, ah
    div cl                                          ; al - quotient, ah - remainder

    mov bh, ah

    mov dl, al
    add dl, '0'
    mov ah, 02h
    int 21h

    mov dl, bh
    add dl, '0'
    mov ah, 02h
    int 21h 
endm

