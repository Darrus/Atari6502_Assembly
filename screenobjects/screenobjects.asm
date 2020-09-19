	processor 6502

	include "vcs.h"
	include "macro.h"

	seg code
	org $F000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start an uninitialized segment at $80 for variable declaration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	seg.u Variables
	org $80
P0Height ds 1			; defines one byte for player 0 height
P1Height ds 1			; defines one byte for player 1 height

Reset:
	CLEAN_START			; macro to safely clear memory and TIA

	ldx #$80			; blue background color
	stx COLUBK

	ldx #%1111			; white playfield color
	stx COLUPF

	lda #10				; A = 10
	sta P0Height		; P0Height = 10
	sta P1Height		; P1Height = 10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the TIA registers for the colors of P0 and P1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #$48			; player 0 color light red
	sta COLUP0

	lda #$C6			; player 1 color light green
	sta COLUP1

	ldy #%00000010		; CTRLPF D1 set to 1 means (score)
	sty CTRLPF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start a new frame by turning on VBLANK and VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:
	lda #2				; same as binary value %00000010
	sta VBLANK			; turn on VBLANK
	sta VSYNC			; turn on VSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate the three lines of VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	REPEAT 3			; DASM Assembler command
		sta WSYNC		; three scanlines for VSYNC
	REPEND				; Dasm Assembler command

	lda #0
	sta VSYNC			; turn off VSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let the TIA output the recommended 37 scanlines of VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	REPEAT 37			; DASM Assembler command
		sta WSYNC		; 37 scanlines for VBLANK
	REPEND				; DASM Assembler command

	lda #0
	sta VBLANK 			; turn off VBLANK

VisibleScanlines:
	; Draw 10 empty scanlines at the top of the frame
	REPEAT 10
		sta WSYNC
	REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display 10 scanlines for the scoreboard number
;; Pull data from an array of bytes defined at NumberBitmap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ldy #0
ScoreboardLoop:
	lda NumberBitmap,y
	sta PF1
	sta WSYNC
	iny					; Increments Y
	cpy	#10				; compares y to decimal value 10
	bne ScoreboardLoop

	lda #0
	sta PF1				; disable playfield

	; Draw 50 empty scanliens between scoreboard and the player
	REPEAT 50
		sta WSYNC
	REPEND

	ldy #0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display 10 scanlines for player 0
;; Pull data from an array of bytes defined at PlayerBitmap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Player0Loop:
	lda PlayerBitmap,y ; load register A from value of PlayerBitmap memory position + Y
	sta GRP0
	sta WSYNC
	iny					; Increments Y
	cpy	P0Height		; compares y to decimal value 10
	bne Player0Loop

	lda #0
	sta GRP0			; disable player 0 graphics

	ldy #0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display 10 scanlines for player 1
;; Pull data from an array of bytes defined at PlayerBitmap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Player1Loop:
	lda PlayerBitmap,y ; load register A from value of PlayerBitmap memory position + Y
	sta GRP1
	sta WSYNC
	iny					; Increments Y
	cpy	P1Height		; compares y to decimal value 10
	bne Player1Loop

	lda #0
	sta GRP1			; disable player 1 graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw the remaining 102 scanlines (192-90), since we already
;; used 10+10+50+10+10=90 scanlines in the current frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	REPEAT 102
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
;; defines an array of bytes to draw the player
;; We add these bytes in the last ROM Addresses.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	org $FFE8
PlayerBitmap:
	.byte #%01111110	;  ######
	.byte #%11111111	; ########
	.byte #%10011001	; #  ##  #
	.byte #%11111111	; ########
	.byte #%11111111	; ########
	.byte #%11111111	; ########
	.byte #%10111101	; # #### #
	.byte #%11000011	; ##    ##
	.byte #%11111111	; ########
	.byte #%01111110	;  ######

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defines an array of bytes to draw the the scoreboard number
;; We add these bytes in the last ROM Addresses.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	org $FFF2
NumberBitmap:
	.byte #%00001110	; ########
	.byte #%00001110	; ########
	.byte #%00000010	;       ##
	.byte #%00000010	;       ##
	.byte #%00001110	; ########
	.byte #%00001110	; ########
	.byte #%00001000	; ##
	.byte #%00001000	; ##    
	.byte #%00001110	; ########
	.byte #%00001110	; ########

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete my ROM size to 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	org $FFFC
	.word Reset
	.word Reset