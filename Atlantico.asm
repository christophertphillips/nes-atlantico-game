.include "IncFiles/consts.inc"
.include "IncFiles/header.inc"
.include "IncFiles/reset.inc"
.include "IncFiles/utils.inc"
.include "IncFiles/Macros/load-init-nametable.inc"
.include "IncFiles/Macros/set-ppu-scroll-zero.inc"
.include "IncFiles/Macros/set-ppu-scroll-x.inc"

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

.include "IncFiles/Procedures/load-palette.inc"
.include "IncFiles/Procedures/load-sprites.inc"
.include "IncFiles/Procedures/read-controllers.inc"
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

  LOAD_INIT_NAMETABLE

EnableRendering:
  SET_PPU_SCROLL_ZERO #%10010000        ; initialize PPU_SCROLL's X,Y values to 0
  lda #%00011110                        ; set PPU_MASK bits to show background
  sta PPU_MASK

GameLoop:                               ; loop forever at end of program
  jmp GameLoop

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
    LoadNewColumnTiles:
      jsr LoadColumnTiles               ; load a new set of column tiles

      lda SourceColIndex                ; else, increment source column index
      clc
      adc #1
      and #$7F                          ; ensure source column index <= 128
      sta SourceColIndex                ; store source column index
:

NewAttributeBlockCheck:
  lda #$1F                              ; has the screen scrolled by 32 pixels/units?
  bit XScroll
  bne :+                                ; if no, skip
    LoadNewAttributeBlocks:
      jsr LoadAttributeBlocks           ; else, load a new set of attribute blocks
:

  ; draw status bar
SetStatusScrollToZero:
  SET_PPU_SCROLL_ZERO #%10010000        ; set PPU_SCROLL's X,Y values to 0 (to "freeze" status bar)

PollSprite0HitReset:                    ; wait for Sprite0Hit to be reset to 0 (end of vblank)
  lda #$40
  bit PPU_STATUS
  bne PollSprite0HitReset

PollSprite0Hit:                         ; wait for Sprite0Hit to be set to 1 (sprite0 hit detected/status bar fully-drawn)
  lda #$40
  bit PPU_STATUS
  beq PollSprite0Hit

  ; draw rest of screen
IncrementXScroll:
  inc XScroll                           ; increment horizontal scroll position
  bne :+                                ; has the edge of the screen been reached?
    SwapNameTable:
      lda CurrNameTable                 ; if yes, swap current 'starting' NameTable index in RAM
      eor #$01
      sta CurrNameTable
:
  SET_PPU_SCROLL_X #%10010000           ; set PPU_SCROLL's X value to XScroll value (and Y to 0)

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
.byte $27,  $50,  %00100001,  $6
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