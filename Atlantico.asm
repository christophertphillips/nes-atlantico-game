.include "IncFiles/consts.inc"
.include "IncFiles/header.inc"
.include "IncFiles/reset.inc"
.include "IncFiles/utils.inc"


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
  sta XScroll                 ; initialize horizontal scroll position to 0
  sta CurrNameTable           ; initialize the 'starting' NameTable

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