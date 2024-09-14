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
.include "IncFiles/Macros/set-check-actor-collision-args.inc"
.include "IncFiles/Macros/set-check-actor-collision-bounds-args.inc"
.include "IncFiles/Structs/actor.inc"
.include "IncFiles/Enums/actor-type.inc"
.include "IncFiles/Macros/set-rendering.inc"
.include "IncFiles/Macros/poll-is-nmi-complete.inc"
.include "IncFiles/Enums/game-state.inc"
.include "IncFiles/Macros/set-switch-chr-bank-args.inc"
.include "IncFiles/Enums/title-screen-selection.inc"

;--------------------------------------------------------
; RAM
;--------------------------------------------------------

.segment "ZEROPAGE"
Score:                .res 4            ; game score (1s-10s-100s-1000s)
Buttons:              .res 1            ; button state
PrevButtons:          .res 1            ; previous button state
Frame:                .res 1            ; # of frames
IsNMIComplete:        .res 1            ; indciate when vblank nmi is done drawing
Clock60:              .res 1            ; # of elapsed seconds
PrevSubmarineClock:   .res 1            ; seconds when previous submarine spawned
PrevAirplaneClock:    .res 1            ; seconds when previous airplane spawned
Seed:                 .res 2            ; seed for generating random bytes
BgPtr:                .res 2            ; pointer to the background address
XScroll:              .res 1            ; horizontal scroll position
CurrNameTable:        .res 1            ; store the current 'starting' NameTable (0 or 1)
SourceColIndex:       .res 1            ; index of source column
DestAddr:             .res 2            ; address of destination column in PPU memory map
SourceAddr:           .res 2            ; address of source column/attribute in ROM
OAMRAMIndex:          .res 1            ; index of OAM RAM data
PrevOAMRAMIndex:      .res 1            ; index of previous OAM RAM data
Collision:            .res 1            ; indicate when a collision has occurred between actors
GameState:            .res 1            ; store the current state of the game
TitleScreenSelection: .res 1            ; store the current title screen selection

AddActor_Type:        .res 1            ; AddActor param (type of actor)
AddActor_XPos:        .res 1            ; AddActor param (X position of actor)
AddActor_YPos:        .res 1            ; AddActor param (Y position of actor)
AddActor_XVel:        .res 1            ; AddActor param (X velocity of actor)
AddActor_YVel:        .res 1            ; AddActor param (Y velocity of actor)

DrawMetaSprite_XPos:      .res 1        ; DrawMetaSprite param (X position of metasprite)
DrawMetaSprite_YPos:      .res 1        ; DrawMetaSprite param (Y position of metasprite)
DrawMetaSprite_TileNum:   .res 1        ; DrawMetaSprite param (starting tile number of metasprite)
DrawMetaSprite_Attribs:   .res 1        ; DrawMetaSprite param (attributes of metasprite)
DrawMetaSprite_TotalTiles:.res 1        ; DrawMetaSprite param (total # of tiles comprising metasprite)

CheckActorCollision_XPos: .res 1        ; CheckActorCollision param (X position of source actor)
CheckActorCollision_YPos: .res 1        ; CheckActorCollision param (Y position of source actor)

CheckActorCollisionBounds_X0:  .res 1   ; CheckActorCollisionBounds param (X0 position of target actor)
CheckActorCollisionBounds_Y0:  .res 1   ; CheckActorCollisionBounds param (Y0 position of target actor)
CheckActorCollisionBounds_X1:  .res 1   ; CheckActorCollisionBounds param (X1 position of target actor)
CheckActorCollisionBounds_Y1:  .res 1   ; CheckActorCollisionBounds param (Y1 position of target actor)

SwitchCHRBank_Bank:            .res 1   ; SwitchCHRBank param

ActorsArray:          .res MAX_ACTORS * .sizeof(Actor) ; array of actors

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
.include "IncFiles/Procedures/spawn-actors.inc"
.include "IncFiles/Procedures/get-random-byte.inc"
.include "IncFiles/Procedures/check-actor-collision.inc"
.include "IncFiles/Procedures/check-actor-collision-bounds.inc"
.include "IncFiles/Procedures/increment-score.inc"
.include "IncFiles/Procedures/render-score.inc"
.include "IncFiles/Procedures/transfer-buffer.inc"
.include "IncFiles/Procedures/load-title-screen.inc"
.include "IncFiles/Procedures/switch-chr-bank.inc"
.include "IncFiles/Procedures/set-title-screen-cursor-position.inc"

Reset:
  INIT_NES

;--------------------------------------------------------
; title screen
;--------------------------------------------------------

Title:
.scope Title

  SET_SWITCH_CHR_BANK_ARGS #0           ; switch to CHR bank 0 (title screen CHR bank)
  jsr SwitchCHRBank

  lda #TitleScreenSelection::CLEARDAY   ; initialize title screen selection to 'clearday'
  sta TitleScreenSelection

SetGameState:
  lda #GameState::TITLE                 ; set game state to title screen
  sta GameState

  jsr LoadPalette                       ; load title screen palette data
  jsr LoadTitleScreen                   ; load title screen nametable

DrawTitleScreenCursor:
  lda #$5E                              ; set y-position, sprite, attributes, and x-position of cursor
  sta OAM_RAM_ADDR+0
  lda #$23
  sta OAM_RAM_ADDR+1
  lda #$01
  sta OAM_RAM_ADDR+2
  lda #$5F
  sta OAM_RAM_ADDR+3

ResetPPUScroll:
  SET_PPU_SCROLL_ZERO #%10010000        ; initialize PPU_SCROLL's X,Y values to 0

EnableRendering:
  SET_RENDERING #%10010000, #%00011110  ; enable NMI interrupts, set background pattern table address = $1000
                                        ; show background, sprites, background/sprites in leftmost 8px

TitleLoop:
  STORE_PREV_BUTTONS                    ; store previously-pressed buttons

  jsr ReadControllers                   ; read controller inputs

  jsr SetTitleScreenCursorPosition      ; set cursor position

CheckStartButton:
  CHECK_BUTTON #BUTTON_START, Buttons   ; has the start button been pressed?
  beq :+                                ; if no, skip
      jmp Game                          ; else, jump to game
  :

CheckUpButton:
  CHECK_BUTTON #BUTTON_UP, Buttons      ; has the up button been pressed?
  beq :+                                ; if no, skip
      CHECK_BUTTON #BUTTON_UP, PrevButtons ; else, was the up button pressed previously?
      bne :+                            ; if yes, skip
          dec TitleScreenSelection      ; else, decrement title screen selection

          lda TitleScreenSelection      ; is the selection at -255?
          cmp #$FF
          bne :+                        ; if no, skip
              lda #2                    ; else, wraparound to 2
              sta TitleScreenSelection
  :

CheckDownButton:
  CHECK_BUTTON #BUTTON_DOWN, Buttons    ; has the down button been pressed?
  beq :+                                ; if no, skip
      CHECK_BUTTON #BUTTON_DOWN, PrevButtons ; else, was the down button pressed previously?
      bne :+                            ; if yes, skip
          inc TitleScreenSelection      ; else, increment title screen selection

          lda TitleScreenSelection      ; is the selection at 3?
          cmp #$03
          bne :+                        ; if no, skip
              lda #0                    ; else, wraparound to 0
              sta TitleScreenSelection
  :

POLL_IS_NMI_COMPLETE TitleLoop          ; wait for NMI to complete before resuming game loop

.endscope

;--------------------------------------------------------
; gameplay
;--------------------------------------------------------

Game:
.scope Game

  SET_SWITCH_CHR_BANK_ARGS #1           ; switch to CHR bank 1 (gameplay CHR bank)
  jsr SwitchCHRBank

SetGameState:
  lda #GameState::GAME                  ; set game state to gameplay
  sta GameState

DisableRendering:
  SET_RENDERING #0, #0

InitVariables:
  lda #0                                ; set frame, IsNMIComplete, clock counters to 0
  sta Buttons
  sta PrevButtons
  sta Frame
  sta IsNMIComplete
  sta Clock60
  sta XScroll                           ; initialize horizontal scroll position to 0
  ;sta CurrNameTable                    ; initialize the 'starting' NameTable
  sta SourceColIndex                    ; initialize the source column index to 0
  sta OAMRAMIndex                       ; initialize the OAM RAM index to 0
  sta PrevOAMRAMIndex
  sta PrevSubmarineClock
  sta PrevAirplaneClock

  lda #$12                              ; initialize seed (both bytes)
  sta Seed+0
  lda #$34
  sta Seed+1

Main:
  jsr LoadPalette                       ; set palette data

AddSprite0:
  SET_ADD_ACTOR_ARGS #ActorType::SPRITE0, #$6, #$27, #$0, #$0 ; add Sprite0 to actors
  jsr AddActor

AddPlayer:
  SET_ADD_ACTOR_ARGS #ActorType::PLAYER, #$70, #$A6, #$0, #$0 ; add Player to actors
  jsr AddActor

  LOAD_INIT_NAMETABLE

ResetPPUScroll:
  SET_PPU_SCROLL_ZERO #%10010000        ; initialize PPU_SCROLL's X,Y values to 0

EnableRendering:
  SET_RENDERING #%10010000, #%00011110  ; enable NMI interrupts, set background pattern table address = $1000
                                        ; show background, sprites, background/sprites in leftmost 8px

  ; game logic loop
GameLoop:
  STORE_PREV_BUTTONS                    ; store previously-pressed buttons

  jsr ReadControllers                   ; read controller inputs

CheckUpButton:
  CHECK_BUTTON #BUTTON_A, Buttons       ; has the A button been pressed?
  beq :+                                ; if no, skip
      CHECK_BUTTON #BUTTON_A, PrevButtons ; else, was the A button pressed previously?
      bne :+                            ; if yes, skip
          SET_ADD_ACTOR_ARGS #ActorType::MISSILE, #$70, #$A6, #$0, #$FF ; else, add Missile to actors
          jsr AddActor
  :

  jsr UpdateActors                      ; update all actors in ActorsArray
  jsr SpawnActors                       ; spawn new actors per conditions
  jsr RenderActors                      ; render all actors in ActorsArray

  POLL_IS_NMI_COMPLETE GameLoop         ; wait for NMI to complete before resuming game loop

.endscope

;--------------------------------------------------------
; NMI interrupt handler
;-------------------------------------------------------- 

NMI:
  STACK_PUSH_ALL                        ; push A,X,Y registers to stack

  inc Frame                             ; increment Frame

OAMStartDMACopy:
  lda #$02                              ; indicate that sprite data to be copied is at $02**
  sta PPU_OAM_DMA                       ; initiate DMA copy (indicating address above)

CheckGameState:
  lda GameState                         ; check if game state = game
  cmp #GameState::GAME
  bne SetGameClock                      ; if no, skip all scroll/nametable buffer logic

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

TransferNameTableBuffer:
  jsr TransferBuffer                    ; transfer nametable buffer contents (e.g. score) to nametable

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

PaletteDataClearDay:
.byte $1C,$0F,$22,$1C, $1C,$36,$21,$0B, $1C,$36,$21,$30, $1C,$0F,$3D,$30 ; Background palette
.byte $1C,$0F,$2D,$10, $1C,$0F,$20,$27, $1C,$2D,$38,$18, $1C,$0F,$1A,$32 ; Sprite palette

PaletteDataOvercast:
.byte $1C,$0F,$22,$1C, $1C,$37,$3D,$0F, $1C,$37,$3D,$30, $1C,$0F,$3D,$30 ; Background palette
.byte $1C,$0F,$2D,$10, $1C,$0F,$20,$27, $1C,$2D,$38,$18, $1C,$0F,$1A,$32 ; Sprite palette

PaletteDataNight:
.byte $0C,$0F,$1C,$0C, $0C,$26,$0C,$0F, $0C,$26,$0C,$2D, $0C,$36,$07,$2D ; Background palette
.byte $0C,$0F,$1D,$2D, $0C,$0F,$20,$27, $0C,$2D,$38,$18, $0C,$0F,$1A,$21 ; Sprite palette

BackgroundData:
.include "IncFiles/Data/backgrounddata.inc"

AttributeData:
.include "IncFiles/Data/attributedata.inc"

TitleScreenNameTable:
.incbin "titlescreen.nam"

.segment "CHARS_TITLE"
.incbin "titlescreen.chr"

.segment "CHARS_GAME"
.incbin "atlantico.chr"

;--------------------------------------------------------
; vectors w/ addresses of handlers
;--------------------------------------------------------

.segment "VECTORS"
.word NMI
.word Reset
.word IRQ