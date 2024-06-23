.include "IncFiles/consts.inc"
.include "IncFiles/header.inc"
.include "IncFiles/reset.inc"
.include "IncFiles/utils.inc"


;--------------------------------------------------------
; ROM-specific constants
;--------------------------------------------------------

MAXSPEED  = 120                         ; Max player speed in 1/256 px/frame
ACCEL     = 2                           ; Movement acceleration in 1/256 px/frame^2
BRAKE     = 2                           ; Movement deceleration in 1/256 px/frame^2

;--------------------------------------------------------
; RAM
;--------------------------------------------------------

.segment "ZEROPAGE"
Buttons:              .res 1            ; [$00] button state
XPos:                 .res 2            ; [$01] player X position, (8.8 fixed-point math), (Xhi + Xlo/256) pixels
YPos:                 .res 2            ; [$03] player Y position, (8.8 fixed-point math), (Yhi + Ylo/256) pixels
XVel:                 .res 1            ; [$05] player X speed in pixels per 256 frames (pixel/256frames)
YVel:                 .res 1            ; [$06] player Y speed in pixels per 256 frames (pixel/256frames)
Frame:                .res 1            ; [$07] # of frames
Clock60:              .res 1            ; [$08] # of elapsed seconds
BgPtr:                .res 2            ; [$09] pointer to the background address
XScroll:              .res 1            ; [$0B] horizontal scroll position
CurrNameTable:        .res 1            ; [$0C] store the current 'starting' NameTable (0 or 1)
SourceColIndex:       .res 1            ; [$0D] index of source column
DestAddr:             .res 2            ; [$0E] address of destination column in PPU memory map
SourceAddr:           .res 2            ; [$10] address of source column/attribute in ROM

;--------------------------------------------------------
; PRG-ROM (at $8000)
;-------------------------------------------------------- 

.segment "CODE"

.proc LoadPalette
  PPU_SETADDR $3F00                     ; set PPU address to $3F00

 ldx #0
Loop:
  lda PaletteData,X                     ; get color value
  sta PPU_DATA                          ; send value to PPU_DATA
  inx
  cpx #32                               ; loop through all 32 colors
  bne Loop

  rts
.endproc

.proc LoadNameTable0
  lda #<NameTable0Data                  ; load lower byte of NameTable[0/1]Data address
  sta BgPtr
  lda #>NameTable0Data                  ; load upper byte of NameTable[0/1]Data address
  sta BgPtr+1

  PPU_SETADDR $2000                     ; set PPU address to $2000

  LOOP_BACKGROUND_DATA                  ; load all nametable data

  rts                                   ; else, return from subroutine
.endproc

.proc LoadNameTable1
  lda #<NameTable1Data                  ; load lower byte of NameTable[0/1]Data address
  sta BgPtr
  lda #>NameTable1Data                  ; load upper byte of NameTable[0/1]Data address
  sta BgPtr+1

  PPU_SETADDR $2400                     ; set PPU address to $2000

  LOOP_BACKGROUND_DATA                  ; load all nametable data

  rts                                   ; else, return from subroutine
.endproc

.proc LoadSprites
  ldx #0
Loop:
  lda SpriteData,X                      ; load tile
  sta $0200,X                           ; send tile to RAM (to be transferred to VRAM via DMA later)
  inx
  cpx #16                               ; check if all tiles have been send to RAM
  bne Loop                              ; if no, loop and store next tile

  rts                                   ; else, return from subroutine
.endproc

.proc ReadControllers
  lda #1                                ; set rightmost of Buttons to 1; use later to determine when Buttons is 'full'
  sta Buttons
  sta JOYPAD_1                          ; strobe latch to 'input mode' via Latch line
  lda #0
  sta JOYPAD_1                          ; strobe latch to 'output mode' via Latch line
Loop:
  lda JOYPAD_1                          ; 1) read bit from Data line and invert its
                                        ; 2) send signal via Clock line to shift bits inside controller
  lsr                                   ; right-shift accumualtor to put the just-read bit into the carry flag
  rol Buttons                           ; rotate-left Buttons to put the carry flag into the rightmost Buttons bit; initial 1 bit moves rightward
  bcc Loop                              ; if initial 1 bit has not reached the carry due to rol, get next controller bit

  rts                                   ; else, return from subroutine
.endproc

.include "IncFiles/Procedures/load-column-tiles.inc"
.include "IncFiles/Procedures/load-attribute-blocks.inc"

Reset:
  INIT_NES

InitVariables:
  lda #0                                ; set frame, clock counters to 0
  sta Frame
  sta Clock60
  sta XScroll                           ; initialize horizontal scroll position to 0
  ;sta CurrNameTable                    ; initialize the 'starting' NameTable
  sta SourceColIndex                    ; initialize the source column index to 0

Main:
  jsr LoadPalette                       ; set palette data
  jsr LoadSprites                       ; set sprites (from tiles)

  lda #1                                ; temporarily set current CurNameTable to 1
  sta CurrNameTable

InitNameTableLoop:
      jsr LoadColumnTiles               ; load a new set of column tiles

    InitAttrBlockLoad:
      lda #$03                          ; is the column index a multiple of 4?
      bit SourceColIndex
      bne :+
          jsr LoadAttributeBlocks       ; if yes, load a new set of attribute block
:
      lda XScroll                       ; increment XScroll by 8 (to load next column on next iteration)
      clc
      adc #8
      sta XScroll

      inc SourceColIndex                ; increment source column index

      lda SourceColIndex                ; have all 32 initial columns been loaded?
      cmp #32
  bne InitNameTableLoop                 ; if no, perform another iteration

  ; lda #0
  ; sta XScroll

  lda #0                                ; revert CurNameTable back to 0
  sta CurrNameTable

EnableRendering:
  lda #%10010100                        ; enable NMI interrupts from PPU, set background to use 2nd pattern table, increment PPU_DATA writes by 32
  sta PPU_CTRL
  lda #$00                              ; disable scroll in X and Y
  sta PPU_SCROLL                        ; X
  sta PPU_SCROLL                        ; Y
  lda #%00011110                        ; set PPU_MASK bits to show background
  sta PPU_MASK

LoopForever:                            ; loop forever at end of program
  jmp LoopForever

;--------------------------------------------------------
; NMI interrupt handler
;-------------------------------------------------------- 

NMI:
  inc Frame                             ; increment Frame

OAMStartDMACopy:
  lda #$02                              ; indicate that sprite data to be copied is at $02**
  sta PPU_OAM_DMA                       ; initiate DMA copy (indicating address above)

NewColumnCheck:
  lda #$07                              ; has the screen scrolled by 8 pixels/units?
  bit XScroll
  bne :+                                ; if no, skip
      jsr LoadColumnTiles               ; load a new set of column tiles

      lda SourceColIndex                ; else, increment source column index
      clc
      adc #1
      and #$7F                          ; ensure source column index <= 128
      sta SourceColIndex                ; store source column index
:

NewAttributeBlockCheck:
 lda #$1F
 bit XScroll
 bne :+
     jsr LoadAttributeBlocks
:

ScrollBackground:
  inc XScroll                           ; increment horizontal scroll position
  bne :+                                ; has the edge of the screen been reached?
      lda CurrNameTable                 ; if yes, swap current 'starting' NameTable index in RAM
      eor #$01
      sta CurrNameTable
: lda #%10010100                        ; enable NMI interrupts from PPU, set background to use 2nd pattern table, increment PPU_DATA writes by 32
  ora CurrNameTable                     ; set 'starting' NameTable
  sta PPU_CTRL                          ; set PPU_CTRL with configs described above
  lda XScroll                           ; set PPU_SCROLL's X value to XScroll value
  sta PPU_SCROLL
  lda #$00                              ; set PPU_SCROLL's Y value to 0
  sta PPU_SCROLL

SetGameClock:
  lda Frame                             ; check if 60 frames have been counted
  cmp #60
  bne SkipClock60Increment              ; if no, skip
    inc Clock60                         ; else, increment Clock60 and reset Frame to 0
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
.byte $1C,$0F,$22,$1C, $1C,$37,$3D,$0F, $1C,$37,$3D,$30, $1C,$0F,$3D,$30 ; Background palette
.byte $1C,$0F,$2D,$10, $1C,$0F,$20,$27, $1C,$2D,$38,$18, $1C,$0F,$1A,$32 ; Sprite palette

NameTable0Data:
.incbin "nametable0.nam"

NameTable1Data:
.incbin "nametable1.nam"

BackgroundData:
.include "backgrounddata.inc"

AttributeData:
.include "attributedata.inc"

SpriteData:
;      Y   tile#  attributes   X
.byte $A6,  $60,  %00000000,  $70       ; $200   _______________
.byte $A6,  $61,  %00000000,  $78       ; $204   \  o o o o o  /   <-- Ship (4 tiles)
.byte $A6,  $62,  %00000000,  $80       ; $208    \___________/
.byte $A6,  $63,  %00000000,  $88       ; $20C

.segment "CHARS"
.incbin "atlantico.chr"

;--------------------------------------------------------
; vectors w/ addresses of handlers
;--------------------------------------------------------

.segment "VECTORS"
.word NMI
.word Reset
.word IRQ