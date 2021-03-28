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
MissileXPos byte           ; Missile X Position
MissileYPos byte           ; Missile Y Position
Score byte                 ; 2-digit score stored as BCD
Timer byte                 ; 2-digit score stored as BCD
Temp byte                  ; auxiliary variable to store temp score values
OnesDigitOffset word       ; lookup table offset for the 1's digit
TensDigitOffset word       ; lookup table offset for the 10's digit
JetSpritePtr word          ; pointer to player0 sprite lookup table
JetColorPtr word           ; pointer to player0 color lookup table
BomberSpritePtr word       ; pointer to player1 sprite lookup table
BomberColorPtr word        ; pointer to player1 color lookup table
JetAnimOffset byte         ; Player sprite frame offset for animation
Random byte                ; random number generated to set enemy position
ScoreSprite byte           ; store the sprite bit pattern for the score
TimerSprite byte           ; store the sprite bit pattern for the timer
TerrainColor byte          ; store the color of the terrain
RiverColor byte            ; store the colore of the river

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JET_HEIGHT = 9             ; player0 sprite height (# rows in lookup table)
BOMBER_HEIGHT = 9          ; player1 sprite height (# rows in lookup table)
DIGITS_HEIGHT = 5          ; scoreboard digit hieght (# rows in lookup table)

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
    lda #%11010100
    sta Random
    
    lda #0
    sta Score
    lda #0
    sta Timer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare a MACRO to check if we should dispaly the missile 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    MAC DRAW_MISSILE
        lda #%00000000
        cpx MissileYPos      ; compare X (current scanline) with missile Y pos
        bne .SkipMissileDraw ; if (X != missile Y position), skip draw
.DrawMissile:
        lda #%00000010
        inc MissileYPos
.SkipMissileDraw:
        sta ENAM0            ; store the correct value in the TIA missile register
        ENDM

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
    REPEAT 32
        sta WSYNC           ; recommended lines of VBLANK
    REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and task performed in the the VBlank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda JetXPos
    ldy #0
    jsr SetObjectXPos      ; set player0 horizontal position

    lda BomberXPos
    ldy #1
    jsr SetObjectXPos      ; set bomber horizontal position

    lda MissileXPos
    ldy #2
    jsr SetObjectXPos      ; set missile horizontal position

    jsr CalculateDigitOffset ; calculate the scoreboard digit lookup table offset

    jsr GenerateJetSound   ; configure and enable our jet engine audio

    sta WSYNC
    sta HMOVE              ; apply the horizontal offsets previously set by the routine

    lda #0
    sta VBLANK             ; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the scoreboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #0                  ; clear TIA registers before each new frame
    sta COLUBK
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    sta CTRLPF

    lda #$1E
    sta COLUPF

    ldx #DIGITS_HEIGHT
.ScoreDigitLoop:
    ldy TensDigitOffset     ; get the tens digit offset for the Score
    lda Digits,Y            ; load the bit pattern from lookup table + Y offset
    and #$F0                ; mask/remove the graphics for the ones digit from the A register
    sta ScoreSprite         ; save the score tens digit pattern in a variable
    
    ldy OnesDigitOffset
    lda Digits,Y
    and #$0F                ; remove the graphics for the tens digit from the A register
    ora ScoreSprite         ; merge it with the saved tens digit sprite
    sta ScoreSprite         ; and save it to ScoreSprite
    sta WSYNC
    sta PF1                 ; update the playfield to display the Score sprite

    ldy TensDigitOffset+1    ; get the tens digit offset for the Score
    lda Digits,Y            ; load the bit pattern from lookup table + Y offset
    and #$F0                ; mask/remove the graphics for the ones digit from the A register
    sta TimerSprite         ; save the score tens digit pattern in a variable
    
    ldy OnesDigitOffset+1
    lda Digits,Y
    and #$0F                ; remove the graphics for the tens digit from the A register
    ora TimerSprite         ; merge it with the saved tens digit sprite
    sta TimerSprite         ; and save it to ScoreSprite

    jsr Sleep12Cycles       ; waste some cycles

    sta PF1                 ; update the playfield for Timer display

    ldy ScoreSprite         ; preload for the next scanline
    sta WSYNC

    sty PF1                 ; update playfield for the score display
    inc TensDigitOffset
    inc TensDigitOffset+1
    inc OnesDigitOffset
    inc OnesDigitOffset+1   ; increment all digits for the next line of data

    jsr Sleep12Cycles

    dex
    sta PF1                 ; update playfield for timer display
    bne .ScoreDigitLoop

    sta WSYNC
    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC
    sta WSYNC
    sta WSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 96 visible scanlines for main game (because 2-line kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameVisibleLine:
    lda TerrainColor        
    sta COLUBK              ; set background color to terrain color
    lda RiverColor         
    sta COLUPF              ; set playfield to river color
    lda #1
    sta CTRLPF              ; enable playfield reflection
    lda #$F0
    sta PF0                 ; setting PF0 bit pattern
    lda #$FC
    sta PF1
    lda #0
    sta PF2

    ldx #85                 ; X counts the number of remaining scanlines
.GameLineLoop:              ; . has no special meaning, tutor uses it to indicate that this segment is within the above segment
    DRAW_MISSILE            ; macro to check if we should draw the missile

.AreWeInsideJetSprite:
    txa                     ; transfer X to A
    sec                     ; Set carry flag before subtraction
    sbc JetYPos             ; subtract sprite Y-coordinate
    cmp #JET_HEIGHT          ; are we inside the sprite height
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
    cmp #BOMBER_HEIGHT       ; are we inside the sprite height
    bcc .DrawBomberSprite   ; if result < SpriteHeight, call the draw routine
    lda #0                  ; else, set lookup index to zero
.DrawBomberSprite:
    tay                     
    lda #%00000101
    sta NUSIZ1              ; stretch bomber sprite
    lda (BomberSpritePtr),Y ; load player0 bitmap data from lookup table
    sta WSYNC
    sta GRP1                ; set graphics for player1
    lda (BomberColorPtr),Y  ; load player0 color data from lookup table
    sta COLUP1              ; set color of player1

    dex
    bne .GameLineLoop

    lda #0
    sta JetAnimOffset       ; always reset animation offset to 0

    sta WSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display Overscan
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
.checkUpPFLimit:
    lda JetYPos
    cmp #75
    beq CheckP0Down          ; if JetYPos == 100, jump to next check
.P0UpPressed:
    inc JetYPos
    lda #0
    sta JetAnimOffset

CheckP0Down:
    lda #%00100000           ; player0 joystick up
    bit SWCHA               
    bne CheckP0Left          ; if bit pattern doesn't match, jump to next 
.checkDownPFLimit:
    lda JetYPos
    cmp #0
    beq CheckP0Left
.P0DownPressed:
    dec JetYPos
    lda #0
    sta JetAnimOffset

CheckP0Left:
    lda #%01000000           ; player0 joystick up
    bit SWCHA               
    bne CheckP0Right         ; if bit pattern doesn't match, jump to next 
.checkLeftPFLimit:
    lda JetXPos
    cmp #35
    beq CheckP0Right
.P0LeftPressed:
    dec JetXPos
    lda #JET_HEIGHT
    sta JetAnimOffset

CheckP0Right:
    lda #%10000000           ; player0 joystick up
    bit SWCHA               
    bne CheckButtonPressed   ; if bit pattern doesn't match, jump to next
.checkRightPFLimit:
    lda JetXPos
    cmp #100
    beq CheckButtonPressed
.P0RightPressed:
    inc JetXPos
    lda #JET_HEIGHT
    sta JetAnimOffset

CheckButtonPressed:
    lda #%10000000
    bit INPT4
    bne EndInputCheck       ; if button is not pressed, skip to end
.ButtonPressed:
    lda JetYPos
    clc
    adc #8                  ; set missile infront of jet
    sta MissileYPos
    lda JetXPos
    clc
    adc #5                  ; set missile in the middle of the jet
    sta MissileXPos
    jsr GenerateMissileSound
EndInputCheck:              ; fallback when no input was performed

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
    jsr GetRandomBomberPos   ; call subroutine for random x-position
.SetScoreValues
    sed                     ; enable BCD(decimal) mode
    lda Timer
    clc
    adc #1
    sta Timer               ; add 1 to timer (BCD does not like INC instruction)
    cld                     ; disable BCD(decimal) mode
EndPositionUpdate:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check for object collision
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckCollisionP0P1:
    lda #%10000000
    bit CXPPMM              ; check CXPPMM bit 7 with the above pattern
    bne .P0P1Collided       ; if collided, game over
    jsr SetBackgroundColor
    jmp CheckCollisionM0P1  ; else, skip to next check
.P0P1Collided:
    jsr GameOver            ; call GameOver subroutine

CheckCollisionM0P1:
    lda #%10000000
    bit CXM0P
    bne .M0P1Collided
    jmp EndCollisionCheck
.M0P1Collided:
    sed
    lda Score
    clc
    adc #1
    sta Score
    cld                     ; increment score using BCD
    lda #0
    sta MissileYPos

EndCollisionCheck:
    sta CXCLR               ; clear all collision checks\

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to start a brand new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp StartFrame          ; loop back to next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate audio for the jet engine sound based on the jet Y-position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GenerateJetSound subroutine
    lda #1
    sta AUDV0               ; volume

    lda JetYPos
    lsr
    lsr
    lsr                     ; divide position by 8
    sta Temp
    lda #31
    sec
    sbc Temp
    sta AUDF0               ; frequency
    
    lda #8
    sta AUDC0               ; control (tone)

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate audio for missile (incomplete)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GenerateMissileSound subroutine
    lda #2
    sta AUDV1               ; volume

    lda #15
    sta AUDF1               ; frequency
    
    lda #31
    sta AUDC1               ; control (tone)

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set playfield and terrain color
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetBackgroundColor subroutine
    lda #$C2                ; green
    sta TerrainColor
    lda #$84                ; blue
    sta RiverColor
    rts

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
;; Game Over subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameOver subroutine
    lda #$30
    sta TerrainColor        ; set terrain color to red
    sta RiverColor          ; set river color to red
    lda #0
    sta Score               ; Score = 0
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to generate a Linear-Feedback Shift Resgistar random number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetRandomBomberPos subroutine
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl
    rol Random              ; performs a series of shifts and bit operations
    lsr
    lsr                     ; divide the value by 4 with 2 right shifts
    sta BomberXPos
    lda #30
    adc BomberXPos
    sta BomberXPos

    lda #96
    sta BomberYPos
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle scoreboard digits to be displayed on the screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert the high and low nibbles of the variable Score and Timer
;; into the offsets of digits lookup table so the values can be displayed.
;; Each digit has a height of 5 bytes in the lookup table.
;;
;; For the low nibble we need to multiple by 5
;;  - we can use left shitsf to perform multiplcation by 2
;;  - for any number N, the value of N*5 = (N*2*2)+N
;;
;; For the upper nibble, since it's already times 16, we need to divide it
;; and then multiply by 5.
;;  - we can use right shifts to perform division by 2
;;  - for any number N, the value of (N/16)*5 = (N/2/2)+(N/2/2/2/2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalculateDigitOffset subroutine
    ldx #1                  ; X register is the loop counter
.PrepareScoreLoop           ; this will loop twice, first X=1, and then X=0
    lda Score,X             ; load A with Timer (X=1) or Score (X=0)
    and #$0F
    sta Temp                ; save the value of A into Temp
    asl
    asl
    adc Temp
    sta OnesDigitOffset,X   ; save A in OnesDigitOffset+1 or OnesDigitOffset+0
    
    lda Score,X             ; load A with Timer (X=1) or Score (X=0)
    and #$F0                ; remove the ones digit by masking 4 bits
    lsr                     ; N/2
    lsr                     ; N/4
    sta Temp                ; Store into temp N/4
    lsr                     ; N/8
    lsr                     ; N/16
    adc Temp                ; add N/16 with value in Temp(N/4)
    sta TensDigitOffset,X   ; save A in TensDigitOffset+1 or TensDigitOffset+0 

    dex
    bpl .PrepareScoreLoop   ; while X >= 0 loop again
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to to waste 12 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jsr takes 6 cycles
;; rts takes 6 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles subroutine
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Digits:
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %00100010          ;  #   #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01100110          ; ##  ##
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #

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