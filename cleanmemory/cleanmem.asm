	processor 6502

	seg code	; defines the segment of code
	org $F000	; defines the code origin at $F000

Start:			; Label start
	sei		; disable interrupts
	cld		; disable the BCD decimal math mode
	ldx #$FF	; loads the X register with #$FF
	txs		; transfer X register to (S)tack register

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Clear the Zero Page region ($00 to $FF)
; Meaning the entire TIA register space and also RAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #0		; A = 0
	ldx #$FF	; X = #$FF
	sta $FF		; Makes sure that FF is zerored 

MemLoop:
	dex		; x--
	sta $0,X	; store A register at address $0 + X
	bne MemLoop	; loop until X == 0 (z-flag set)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Fill ROM size to exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	org $FFFC
	.word Start	; reset vector at $FFFC (where program starts)
	.word Start	; interrupt vector at $FFFE (unused in VCS)
