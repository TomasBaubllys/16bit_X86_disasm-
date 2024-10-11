CMD_ARG_LEN_PTR = 80h
CMD_ARG_START_PTR = 81h

MAX_FILE_NAME_LEN = 0Ch			; read at most 13 bytes

OPC_REG_NAME_LEN = 03h
OPC_REG_COUNT = 10h

OPC_SR_NAME_LEN = 03h

OPC_1100_AE_1011_COUNT = 04h

OPC_1111_011_COUNT = 07h

OPC_0111_COUNT = 10h

OPC_0100_COUNT = 02h

OPC_1001_COUNT = 09h

OPC_1101_00_COUNT = 07h

OPC_1000_00_COUNT = 08h

OPC_1111_0000_TO_0101_1000_TO_1101_COUNT = 0Bh

OPC_0101_COUNT = 02h

OPC_1000_010x_to_111x_COUNT = 06h

; split into two arrays size of 2 total = 4
OPC_1000_ELSE_COUNT = 02h

OPC_1111_111_COUNT = 07h

;; input buffer
BUFFER_IN_LEN = 255

BYTE_BUFFER_LEN = 0Dh

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
