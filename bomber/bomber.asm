    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include VCS register memory and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "vcs.h"
    include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables declaration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u Variables
    org $80

JetXPos byte               ; Player X position
JetYPos byte               ; Player Y Position
BomberXPos byte            ; Enemy X Position
BomberYPos byte            ; Enemy Y Position
JetSpritePtr word          ; pointer to player0 sprite lookup table
JetColorPtr word           ; pointer to player0 color lookup table
BomberSpritePtr word       ; pointer to player1 sprite lookup table
BomberColorPtr word        ; pointer to player1 color lookup table
JetAnimOffset byte         ; Player sprite frame offset for animation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JET_HEIGHT = 9             ; player0 sprite height (# rows in lookup table)
BOMBER_HEIGHT = 9          ; player1 sprite height (# rows in lookup table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg Code
    org $F000

Reset:
    CLEAN_START             ; Call macro to reset memory and registers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize RAM variables and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #10
    sta JetYPos
    lda #90
    sta JetXPos
    lda #80
    sta BomberYPos
    lda #60
    sta BomberXPos

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize pointers to the correct lookup table addresses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #<JetSprite
    sta JetSpritePtr        ; lo-byte pointer for jet sprite lookup table
    lda #>JetSprite
    sta JetSpritePtr+1      ; hi-byte pointer for jet sprite lookup table

    lda #<JetColor
    sta JetColorPtr         ; lo-byte pointer for jet color lookup table
    lda #>JetColor
    sta JetColorPtr+1       ; hi-byte pointer for jet color lookup table

    lda #<BomberSprite
    sta BomberSpritePtr     ; lo-byte pointer for enemy sprite lookup table
    lda #>BomberSprite
    sta BomberSpritePtr+1   ; hi-byte pointer for enemy sprite lookup table

    lda #<BomberColor
    sta BomberColorPtr      ; lo-byte pointer for enemy color lookup table
    lda #>BomberColor
    sta BomberColorPtr+1    ; hi-byte pointer for enemy color lookup table

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start the main display loop and frame rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and task performed in the the pre-VBlank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda JetXPos
    ldy #0
    jsr SetObjectXPos      ; set player0 horizontal position

    lda BomberXPos
    ldy #1
    jsr SetObjectXPos      ; set bomber horizontal position

    sta WSYNC
    sta HMOVE              ; apply the horizontal offsets previously set by the routine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK              ; turn on VBLANK
    sta VSYNC               ; turn on VSYNC
    REPEAT 3
        sta WSYNC           ; recommended lines of VSYNC
    REPEND
    lda #0 
    sta VSYNC               ; turn off VSYNC
    REPEAT 37
        sta WSYNC           ; recommended lines of VBLANK
    REPEND
    sta VBLANK              ; turn of VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 96 visible scanlines for main game (because 2-line kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameVisibleLine:
    lda #$84
    sta COLUBK              ; set background color to blue
    lda #$C2                
    sta COLUPF              ; set playfield to green

    lda #1
    sta CTRLPF              ; enable playfield reflection

    lda #$F0
    sta PF0                 ; setting PF0 bit pattern
    
    lda #$FC
    sta PF1

    lda #0
    sta PF2

    ldx #96                ; X counts the number of remaining scanlines

.GameLineLoop:              ; . has no special meaning, tutor uses it to indicate that this segment is within the above segment
.AreWeInsideJetSprite:
    txa                     ; transfer X to A
    sec                     ; Set carry flag before subtraction
    sbc JetYPos             ; subtract sprite Y-coordinate
    cmp JET_HEIGHT          ; are we inside the sprite height
    bcc .DrawJetSprite      ; if result < SpriteHeight, call the draw routine
    lda #0                  ; else, set lookup index to zero
.DrawJetSprite:
    clc                     ; clear carry flag before addition
    adc JetAnimOffset       ; offset animation to jump to correct frame 

    tay                     
    lda (JetSpritePtr),Y    ; load player0 bitmap data from lookup table
    sta WSYNC
    sta GRP0                ; set graphics for player0
    lda (JetColorPtr),Y     ; load player0 color data from lookup table
    sta COLUP0              ; set color of player0
.AreWeInsideBomber:
    txa                     ; transfer X to A
    sec                     ; Set carry flag before subtraction
    sbc BomberYPos          ; subtract sprite Y-coordinate
    cmp BOMBER_HEIGHT       ; are we inside the sprite height
    bcc .DrawBomberSprite   ; if result < SpriteHeight, call the draw routine
    lda #0                  ; else, set lookup index to zero
.DrawBomberSprite:
    tay                     
    
    lda #%00000101
    sta NUSIZ1              ; stretch bomber sprite

    lda (BomberSpritePtr),Y ; load player0 bitmap data from lookup table
    sta WSYNC
    sta GRP1                ; set graphics for player0
    lda (BomberColorPtr),Y  ; load player0 color data from lookup table
    sta COLUP1              ; set color of player0


    dex
    bne .GameLineLoop

    lda #0
    sta JetAnimOffset       ; always reset animation offset to 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispaly Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK              ; turn VBLANK on
    REPEAT 30
        sta WSYNC           ; recommended lines of VBLANK
    REPEND
    lda #0
    sta VBLANK              ; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process joystick input for player0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Up:
    lda #%00010000           ; player0 joystick up
    bit SWCHA               
    bne CheckP0Down          ; if bit pattern doesn't match, jump to next check
    inc JetYPos
    lda #0
    sta JetAnimOffset

CheckP0Down:
    lda #%00100000           ; player0 joystick up
    bit SWCHA               
    bne CheckP0Left          ; if bit pattern doesn't match, jump to next 
    dec JetYPos
    lda #0
    sta JetAnimOffset

CheckP0Left:
    lda #%01000000           ; player0 joystick up
    bit SWCHA               
    bne CheckP0Right         ; if bit pattern doesn't match, jump to next 
    dec JetXPos
    lda JET_HEIGHT
    sta JetAnimOffset

CheckP0Right:
    lda #%10000000           ; player0 joystick up
    bit SWCHA               
    bne EndInputCheck        ; if bit pattern doesn't match, jump to 
    inc JetXPos
    lda JET_HEIGHT
    sta JetAnimOffset

EndInputCheck:               ; fallback when no input was performed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations to update position for next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpdateBomberPosition:
    lda BomberYPos
    clc
    cmp #0
    bmi .ResetBomberPosition ; if < 0, reset y-position
    dec BomberYPos           ; else decrement y-position
    jmp EndPositionUpdate    ; jump to EndPositionUpdate to bypass reset
.ResetBomberPosition
    lda #96
    sta BomberYPos
EndPositionUpdate:
   
    jmp StartFrame           ; loop back to next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle object horizontal position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A is the target x-coord position in pixels of our object
;; Y is the object type (0:player, 1:bomber, 2:missle0, 3:missile1, 4:ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectXPos subroutine
    sta WSYNC
    sec
.Div15Loop
    sbc #15
    bcs .Div15Loop           ; loop until carry-flag is clear
    eor #7
    asl
    asl
    asl
    asl                     ; four shift lefts to get only the top 4 bits
    sta HMP0,Y              ; store the fine offset to the correct HMxx
    sta RESP0,Y             ; fix object position in 15-step increment
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JetSprite:
    .byte #%00000000         ;
    .byte #%00010100         ;   # #
    .byte #%01111111         ; #######
    .byte #%00111110         ;  #####
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #

JetSpriteTurn:
    .byte #%00000000         ;
    .byte #%00001000         ;    #
    .byte #%00111110         ;  #####
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #

BomberSprite:
    .byte #%00000000         ;
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00101010         ;  # # #
    .byte #%00111110         ;  #####
    .byte #%01111111         ; #######
    .byte #%00101010         ;  # # #
    .byte #%00001000         ;    #
    .byte #%00011100         ;   ###

JetColor:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$BA
    .byte #$0E
    .byte #$08

JetColorTurn:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$0E
    .byte #$0E
    .byte #$08

BomberColor:
    .byte #$00
    .byte #$32
    .byte #$32
    .byte #$0E
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM size with exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC
    word Reset              ; write 2 bytes with the program reset address
    word Reset              ; write 2 bytes with the interruption vector