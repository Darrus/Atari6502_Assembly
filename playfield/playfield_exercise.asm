	processor 6502

	include "vcs.h"
	include "macro.h"

	seg code
	org $F000

Reset:
	CLEAN_START		; macro to safely clear memory and TIA

	ldx #$80		; blue background color
	stx COLUBK

	ldx #$1C		; yellow playfield color
	stx COLUPF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start a new frame by turning on VBLANK and VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:
	lda #2			; same as binary value %00000010
	sta VBLANK		; turn on VBLANK
	sta VSYNC		; turn on VSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate the three lines of VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	REPEAT 3		; DASM Assembler command
		sta WSYNC	; three scanlines for VSYNC
	REPEND			; Dasm Assembler command
	lda #0
	sta VSYNC		; turn off VSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let the TIA output the recommended 37 scanlines of VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	REPEAT 37		; DASM Assembler command
		sta WSYNC	; 37 scanlines for VBLANK
	REPEND			; DASM Assembler command
	lda #0
	sta VBLANK 		; tirm pff VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the CTRLPF register to allow playfield reflection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ldx #1		; CTRLPF register (D0 means reflect the PF)
	stx CTRLPF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw the 192 visible scanlines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Skip 7 scanlines with no PF set
	ldx #0
	stx PF0
	stx PF1
	stx PF2
	REPEAT 7
		sta WSYNC
	REPEND

	; Set the PF0 to 1110 (LSB first) and PF1-PF2 as 1111 1111
	ldx #%11100000
	stx PF0
	ldx #%11111111
	stx PF1
	stx PF2
	REPEAT 7
		sta WSYNC
	REPEND

	; Set the next 164 lines only with PF0 third bit enabled
	ldx #%01100000
	stx PF0
	ldx #%00000000
	stx PF1
        ldx #%10000000
	stx PF2
	REPEAT 164
		sta WSYNC
	REPEND
	
	; Set the PF0 to 1110 (LSB first) and PF1-PF2 as 1111 1111
	ldx #%11100000
	stx PF0
	ldx #%11111111
	stx PF1
	stx PF2
	REPEAT 7
		sta WSYNC
	REPEND

	; Skip 7 scanlines with no PF set
	ldx #0
	stx PF0
	stx PF1
	stx PF2
	REPEAT 7
		sta WSYNC
	REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output 30 more VBLANK overscan lines to complete our frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #2
	sta VBLANK
	REPEAT 30
		sta WSYNC
	REPEND
	lda #0
	sta VBLANK

	jmp StartFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete my ROM size to 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	org $FFFC
	.word Reset
	.word Reset