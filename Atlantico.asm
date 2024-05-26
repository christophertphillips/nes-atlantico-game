.include "IncFiles/consts.inc"
.include "IncFiles/header.inc"
.include "IncFiles/reset.inc"
.include "IncFiles/utils.inc"

;--------------------------------------------------------
; ROM-specific macros
;--------------------------------------------------------

.macro PERFORM_POSITIVE_ACCEL
  lda XVel                              ; load current XVel
  clc                                   ; add ACCEL to XVel
  adc #ACCEL
  cmp #MAXSPEED                         ; is new XVel > MAXSPEED?
  bcc :+                                ; if no, continue with new XVel
      lda #MAXSPEED                     ; else, new XVel = MAXSPEED
: sta XVel                              ; store new XVel
.endmacro

.macro PERFORM_NEGATIVE_ACCEL
  lda XVel                              ; load current XVel
  sec                                   ; subtract ACCEL from XVel
  sbc #ACCEL
  cmp #256-MAXSPEED                     ; is new XVel < MAXSPEED?
  bcs :+                                ; if no, continue with new XVel
      lda #256-MAXSPEED                 ; else, new XVel = MAXSPEED
  : sta XVel                            ; store new XVel
.endmacro

.macro PERFORM_POSITIVE_DECEL
  lda XVel                              ; load current XVel
  sec                                   ; subtract BRAKE from XVel
  sbc #BRAKE
  ; cmp #0                              ; is new XVel > 0?
  bpl :+                                ; if yes, continue with current XVel
      lda #0                            ; else, new XVel = 0
  : sta XVel                            ; store new XVel
.endmacro

.macro PERFORM_NEGATIVE_DECEL
  lda XVel                      ; load current XVel
  clc                           ; subtract BRAKE from XVel
  adc #BRAKE
  ; cmp #0                      ; is new XVel < 0?
  bmi :+                        ; if yes, continue with current XVel
      lda #0                    ; else, new XVel = 0
  : sta XVel
.endmacro

;--------------------------------------------------------
; ROM-specific constants
;--------------------------------------------------------

MAXSPEED  = 120               ; Max player speed in 1/256 px/frame
ACCEL     = 2                 ; Movement acceleration in 1/256 px/frame^2
BRAKE     = 2                 ; Movement deceleration in 1/256 px/frame^2

;--------------------------------------------------------
; RAM
;--------------------------------------------------------

.segment "ZEROPAGE"
Buttons:              .res 1  ; button state
XPos:                 .res 2  ; player X position, (8.8 fixed-point math), (Xhi + Xlo/256) pixels
YPos:                 .res 2  ; player Y position, (8.8 fixed-point math), (Yhi + Ylo/256) pixels
XVel:                 .res 1  ; player X speed in pixels per 256 frames (pixel/256frames)
YVel:                 .res 1  ; player Y speed in pixels per 256 frames (pixel/256frames)
TileOffset:           .res 1  ; tile offset (0 or 4)
Frame:                .res 1  ; # of frames
Clock60:              .res 1  ; # of elapsed seconds
BgPtr:                .res 2  ; pointer to the background address
XScroll:              .res 1  ; horizontal scroll position
CurrNameTable:        .res 1  ; store the current 'starting' NameTable (0 or 1)

;--------------------------------------------------------
; PRG-ROM (at $8000)
;-------------------------------------------------------- 

.segment "CODE"

.proc LoadPalette
  PPU_SETADDR $3F00           ; set PPU address to $3F00

 ldx #0
Loop:
  lda PaletteData,X           ; get color value
  sta PPU_DATA                ; send value to PPU_DATA
  inx
  cpx #32                     ; loop through all 32 colors
  bne Loop

  rts
.endproc

.proc LoadNameTable0
  lda #<NameTable0Data        ; load lower byte of NameTable[0/1]Data address
  sta BgPtr
  lda #>NameTable0Data        ; load upper byte of NameTable[0/1]Data address
  sta BgPtr+1

  PPU_SETADDR $2000           ; set PPU address to $2000

  LOOP_BACKGROUND_DATA         ; load all nametable data

  rts                         ; else, return from subroutine
.endproc

.proc LoadNameTable1
  lda #<NameTable1Data        ; load lower byte of NameTable[0/1]Data address
  sta BgPtr
  lda #>NameTable1Data        ; load upper byte of NameTable[0/1]Data address
  sta BgPtr+1

  PPU_SETADDR $2400           ; set PPU address to $2000

  LOOP_BACKGROUND_DATA         ; load all nametable data

  rts                         ; else, return from subroutine
.endproc

.proc LoadSprites
  ldx #0
Loop:
  lda SpriteData,X          ; load tile
  sta $0200,X               ; send tile to RAM (to be transferred to VRAM via DMA later)
  inx
  cpx #16                   ; check if all tiles have been send to RAM
  bne Loop                  ; if no, loop and store next tile

  rts                       ; else, return from subroutine
.endproc

.proc ReadControllers
  lda #1                    ; set rightmost of Buttons to 1; use later to determine when Buttons is 'full'
  sta Buttons
  sta JOYPAD_1              ; strobe latch to 'input mode' via Latch line
  lda #0
  sta JOYPAD_1              ; strobe latch to 'output mode' via Latch line
Loop:
  lda JOYPAD_1              ; 1) read bit from Data line and invert its
                            ; 2) send signal via Clock line to shift bits inside controller
  lsr                       ; right-shift accumualtor to put the just-read bit into the carry flag
  rol Buttons               ; rotate-left Buttons to put the carry flag into the rightmost Buttons bit; initial 1 bit moves rightward
  bcc Loop                  ; if initial 1 bit has not reached the carry due to rol, get next controller bit

  rts                       ; else, return from subroutine
.endproc

Reset:
  INIT_NES

InitVariables:
  lda #0                      ; set frame, clock counters to 0
  sta Frame
  sta Clock60
  sta TileOffset              ; initialize tile offset to 0
  sta XScroll                 ; initialize horizontal scroll position to 0
  sta CurrNameTable           ; initialize the 'starting' NameTable

  lda SpriteData+3            ; load default XPos of player sprite
  sta XPos+1                  ; store hi byte of position (which represents whole pixel count)
  lda SpriteData              ; load default YPos of player sprite
  sta YPos+1                  ; store hi byte of position (which represents whole pixel count)

Main:
  jsr LoadPalette             ; set palette data
  jsr LoadNameTable0          ; set NameTable0 data
  jsr LoadNameTable1          ; set NameTable1 data
  jsr LoadSprites             ; set sprites (from tiles)

EnableRendering:
  lda #%10010000              ; enable NMI interrupts from PPU and set background to use 2nd pattern table
  sta PPU_CTRL
  lda #$00                    ; disable scroll in X and Y
  sta PPU_SCROLL              ; X
  sta PPU_SCROLL              ; Y
  lda #%00011110              ; set PPU_MASK bits to show background
  sta PPU_MASK

LoopForever:                  ; loop forever at end of program
  jmp LoopForever

;--------------------------------------------------------
; NMI interrupt handler
;-------------------------------------------------------- 

NMI:
  inc Frame                   ; increment Frame

OAMStartDMACopy:
  lda #$02                    ; indicate that sprite data to be copied is at $02**
  sta PPU_OAM_DMA             ; initiate DMA copy (indicating address above)

ScrollBackground:
  inc XScroll                 ; increment horizontal scroll position
  lda XScroll
  bne :+                      ; has the edge of the screen been reached?
      lda CurrNameTable       ; if yes, swap current 'starting' NameTable index in RAM
      eor #$01
      sta CurrNameTable
      lda #%10010000          ; enable NMI interrupts from PPU and set background to use 2nd pattern table
      ora CurrNameTable       ; set 'starting' NameTable
      sta PPU_CTRL
      lda XScroll             ; load XScroll (=0) again
: sta PPU_SCROLL              ; set PPU_SCROLL's X value to XScroll value
  lda #$00                    ; set PPU_SCROLL's Y value to 0
  sta PPU_SCROLL

ReadInputs:
  jsr ReadControllers         ; read controller inputs

CheckRightLeftButtons:
  CHECK_BUTTON #BUTTON_RIGHT            ; is right button pressed?
  bne RightButtonPressed                ; if YES, jump to RightButtonPressed
  CHECK_BUTTON #BUTTON_LEFT             ; is left button pressed?
  bne LeftButtonPressed                 ; if YES, jump to LeftButtonPressed
                                        ; else, no buttons prssed; fall throught to NoButtonsPressed
NoButtonsPressed:
  lda XVel                              ; is the current XVel < 0?
  bmi ApplyNegativeDecel                ; if yes, tank is negative-velocity; jump to ApplyNegativeDecel
  beq UpdateMetaSpritePosition          ; else if XVel = 0, tank is "idling"; jump to UpdateMetaSpritePosition
                                        ; else, tank is positive-velocity; fall through to ApplyPositiveDecel

ApplyPositiveDecel:                     ; apply positive deceleration to positive-velocity tank
  PERFORM_POSITIVE_DECEL                ; apply positive deceleration to positive-velocity tank
  jmp UpdateMetaSpritePosition          ; done with motion update; jump to UpdateMetaSpritePosition

ApplyNegativeDecel:
  PERFORM_NEGATIVE_DECEL                ; apply negative deceleration to negative-velocity tank
  jmp UpdateMetaSpritePosition          ; done with motion update; jump to UpdateMetaSpritePosition

RightButtonPressed:
  lda XVel                              ; is the current XVel < 0?
  bmi ApplyNegativeDecel                ; if YES, tank is negative-velocity; jump to ApplyNegativeDecel
                                        ; else, v >= 0; fall through to ApplyPositiveAccel
ApplyPositiveAccel:
  PERFORM_POSITIVE_ACCEL                ; apply positive acceleration to stationary tank
  jmp UpdateMetaSpritePosition          ; done with motion update; jump to UpdateMetaSpritePosition

LeftButtonPressed:
  lda XVel                              ; is the current XVel > 0?
  beq :+
    bpl ApplyPositiveDecel              ; if YES, tank is positive-velocity; jump to ApplyPositiveDecel
                                        ; else, v >= 0; fall through to ApplyNegativeAccel
ApplyNegativeAccel:
: PERFORM_NEGATIVE_ACCEL                ; apply negative acceleration to negative-velocity tank
  ;jmp UpdateMetaSpritePosition         ; done with motion update; jump to UpdateMetaSpritePosition

UpdateMetaSpritePosition:
  lda XVel                    ; load XVel
  bpl :+
    dec XPos+1                ; if velocity is negative, decrement 1 (i.e., add $FF in two's complement) from hi-byte to sign-extend
: clc                         ; add XVel to XPos lo byte
  adc XPos
  sta XPos                    ; store new XPos lo byte
  lda #0                      ; if a carry occurred when incrementing XPos lo byte, add to XPos hi byte
  adc XPos+1
  sta XPos+1                  ; store new XPos hi byte

UpdateHardwareSprites:
  lda XPos+1                  ; load hi byte of X position (which represents whole pixel count)
  sta $0200+3                 ; set the first player sprite X position to be XPos
  sta $0208+3                 ; set the third player sprite X position to be XPos
  clc
  adc #8
  sta $0204+3                 ; set the second player sprite X position to be XPos+8
  sta $020C+3                 ; set the fourth player sprite X position to be XPos+8

  lda YPos+1                  ; load hi byte of Y position (which represents whole pixel count)
  sta $0200                   ; set the first player sprite Y position to be YPos
  sta $0204                   ; set the second player sprite Y position to be YPos
  clc
  adc #8
  sta $0208                   ; set the third player sprite Y position to be YPos
  sta $020C                   ; set the fourth player sprite Y position to be YPos

  lda XPos+1                  ; load hi byte of X position (which represents whole pixel count)
  and #$01                    ; mask all bits except least significant bit
  asl                         ; shift least-sig bit to 2^2=4 position
  asl
  sta TileOffset              ; store offset value (0 or 4)

ApplyHardwareSpriteOffsets:
  lda #$18                    ; load default tile# for first sprite of metasprite
  clc                         ; add offset (0 or 4)
  adc TileOffset
  sta $0201                   ; store tile#

  lda #$1A                    ; load default tile# for second sprite of metasprite
  clc                         ; add offset (0 or 4)
  adc TileOffset
  sta $0205                   ; store tile#

  lda #$19                    ; load default tile# for third sprite of metasprite
  clc                         ; add offset (0 or 4)
  adc TileOffset
  sta $0209                   ; store tile#

  lda #$1B                    ; load default tile# for fourth sprite of metasprite
  clc                         ; add offset (0 or 4)
  adc TileOffset
  sta $020D                   ; store tile#

SetGameClock:
  lda Frame                   ; check if 60 frames have been counted
  cmp #60
  bne SkipClock60Increment    ; if no, skip
    inc Clock60               ; else, increment Clock60 and reset Frame to 0
    lda #0
    sta Frame
SkipClock60Increment:

  rti

;--------------------------------------------------------
; IRQ interrupt handler
;--------------------------------------------------------

IRQ:
  rti

;--------------------------------------------------------
; graphics data (palettes, CHRs, nametable data, etc.)
;--------------------------------------------------------

PaletteData:
.byte $1D,$10,$20,$21, $1D,$1D,$2D,$24, $1D,$0C,$19,$1D, $1D,$06,$17,$07 ; Background palette
.byte $0F,$1D,$19,$29, $0F,$08,$18,$38, $0F,$0C,$1C,$3C, $0F,$2D,$10,$30 ; Sprite palette

NameTable0Data:
.incbin "nametable0.nam"

NameTable1Data:
.incbin "nametable1.nam"

SpriteData:
;       Y   tile#   attribs      X
.byte  $80,   $18,  %00000000,  $10  ; OAM sprite 1
.byte  $80,   $1A,  %00000000,  $18  ; OAM sprite 2
.byte  $88,   $19,  %00000000,  $10  ; OAM sprite 3
.byte  $88,   $1B,  %00000000,  $18  ; OAM sprite 4

.segment "CHARS"
.incbin "battle.chr"

;--------------------------------------------------------
; vectors w/ addresses of handlers
;--------------------------------------------------------

.segment "VECTORS"
.word NMI
.word Reset
.word IRQ