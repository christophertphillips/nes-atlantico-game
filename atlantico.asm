.include "IncFiles/consts.inc"
.include "IncFiles/header.inc"
.include "IncFiles/reset.inc"
.include "IncFiles/utils.inc"
.include "IncFiles/Macros/load-init-nametable.inc"
.include "IncFiles/Macros/set-ppu-scroll-zero.inc"
.include "IncFiles/Macros/set-ppu-scroll-x.inc"
.include "IncFiles/Macros/stack-ops.inc"
.include "IncFiles/Macros/set-add-actor-args.inc"
.include "IncFiles/Macros/set-draw-meta-sprite-args.inc"
.include "IncFiles/Structs/actor.inc"
.include "IncFiles/Enums/actor-type.inc"

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
PrevButtons:          .res 1            ; [$01] previous button state
XPos:                 .res 2            ; [$02] player X position, (8.8 fixed-point math), (Xhi + Xlo/256) pixels
YPos:                 .res 2            ; [$04] player Y position, (8.8 fixed-point math), (Yhi + Ylo/256) pixels
XVel:                 .res 1            ; [$06] player X speed in pixels per 256 frames (pixel/256frames)
YVel:                 .res 1            ; [$07] player Y speed in pixels per 256 frames (pixel/256frames)
Frame:                .res 1            ; [$08] # of frames
IsNMIComplete:        .res 1            ; [$09] indciate when vblank nmi is done drawing
Clock60:              .res 1            ; [$0A] # of elapsed seconds
BgPtr:                .res 2            ; [$0B] pointer to the background address
XScroll:              .res 1            ; [$0D] horizontal scroll position
CurrNameTable:        .res 1            ; [$0E] store the current 'starting' NameTable (0 or 1)
SourceColIndex:       .res 1            ; [$0F] index of source column
DestAddr:             .res 2            ; [$10] address of destination column in PPU memory map
SourceAddr:           .res 2            ; [$12] address of source column/attribute in ROM
OAMRAMIndex:          .res 1            ; [$14] index of OAM RAM data

AddActor_Type:        .res 1            ; [$15] AddActor param (type of actor)
AddActor_XPos:        .res 1            ; [$16] AddActor param (X position of actor)
AddActor_YPos:        .res 1            ; [$17] AddActor param (Y position of actor)
AddActor_XVel:        .res 1            ; [$18] AddActor param (X velocity of actor)
AddActor_YVel:        .res 1            ; [$19] AddActor param (Y velocity of actor)

DrawMetaSprite_XPos:      .res 1        ; [$1A] DrawMetaSprite param (X position of metasprite)
DrawMetaSprite_YPos:      .res 1        ; [$1B] DrawMetaSprite param (Y position of metasprite)
DrawMetaSprite_TileNum:   .res 1        ; [$1C] DrawMetaSprite param (starting tile number of metasprite)
DrawMetaSprite_Attribs:   .res 1        ; [$1D] DrawMetaSprite param (attributes of metasprite)
DrawMetaSprite_TotalTiles:.res 1        ; [$1E] DrawMetaSprite param (total # of tiles comprising metasprite)

ActorsArray:          .res MAX_ACTORS * .sizeof(Actor) ; [$1F] array of actors

;--------------------------------------------------------
; PRG-ROM (at $8000)
;-------------------------------------------------------- 

.segment "CODE"

.include "IncFiles/Procedures/load-palette.inc"
.include "IncFiles/Procedures/read-controllers.inc"
.include "IncFiles/Procedures/load-column-tiles.inc"
.include "IncFiles/Procedures/load-attribute-blocks.inc"
.include "IncFiles/Procedures/draw-metasprite.inc"
.include "IncFiles/Procedures/render-actors.inc"
.include "IncFiles/Procedures/add-actor.inc"
.include "IncFiles/Procedures/update-actors.inc"

Reset:
  INIT_NES

InitVariables:
  lda #0                                ; set frame, IsNMIComplete, clock counters to 0
  sta PrevButtons
  sta Frame
  sta IsNMIComplete
  sta Clock60
  sta XScroll                           ; initialize horizontal scroll position to 0
  ;sta CurrNameTable                    ; initialize the 'starting' NameTable
  sta SourceColIndex                    ; initialize the source column index to 0
  sta OAMRAMIndex                       ; initialize the OAM RAM index to 0

Main:
  jsr LoadPalette                       ; set palette data

AddSprite0:
  SET_ADD_ACTOR_ARGS #ActorType::SPRITE0, #$6, #$27, #$0, #$0 ; add Sprite0 to actors
  jsr AddActor

AddPlayer:
  SET_ADD_ACTOR_ARGS #ActorType::PLAYER, #$70, #$A6, #$0, #$0 ; add Player to actors
  jsr AddActor

  LOAD_INIT_NAMETABLE

EnableRendering:
  SET_PPU_SCROLL_ZERO #%10010000        ; initialize PPU_SCROLL's X,Y values to 0
  lda #%00011110                        ; set PPU_MASK bits to show background
  sta PPU_MASK

  ; game logic loop
GameLoop:
  jsr ReadControllers                   ; read controller inputs

CheckUpButton:
  CHECK_BUTTON #BUTTON_A                ; has the A button been pressed?
  beq :+                                ; if no, skip
      SET_ADD_ACTOR_ARGS #ActorType::MISSILE, #$70, #$A6, #$0, #$FF ; else, add Missile to actors
      jsr AddActor
  :

  jsr UpdateActors                      ; update all actors in ActorsArray
  jsr RenderActors                      ; render all actors in ActorsArray

PollIsNMIComplete:                      ; wait for IsNMIComplete to be set (=1)
      lda IsNMIComplete
      beq PollIsNMIComplete
ResetIsNMIComplete:
  lda #0                                ; reset IsNMIComplete flag
  sta IsNMIComplete
  jmp GameLoop                          ; return to top of GameLoop

;--------------------------------------------------------
; NMI interrupt handler
;-------------------------------------------------------- 

NMI:
  STACK_PUSH_ALL                        ; push A,X,Y registers to stack

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

SetNMIComplete:
  lda #1                                ; set IsNMIComplete flag
  sta IsNMIComplete

  STACK_PULL_ALL                        ; pull A,X,Y registers from stack

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

BackgroundData:
.include "IncFiles/Data/backgrounddata.inc"

AttributeData:
.include "IncFiles/Data/attributedata.inc"

.segment "CHARS"
.incbin "atlantico.chr"

;--------------------------------------------------------
; vectors w/ addresses of handlers
;--------------------------------------------------------

.segment "VECTORS"
.word NMI
.word Reset
.word IRQ