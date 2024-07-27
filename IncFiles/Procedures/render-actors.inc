.proc RenderActors
  ldx #0                                ; index to reference actors
  lda #0                                ; reset OAM RAM index to zero (i.e. $0200 in RAM)
  sta OAMRAMIndex

ActorsLoop:
    lda ActorsArray+Actor::Type,X       ; load type of currently-referenced actor

    cmp #ActorType::SPRITE0             ; is the actor sprite0?
    bne :+                              ; if no, skip; else, proceed
      RenderSprite0:
        lda ActorsArray+Actor::XPos,X   ; store metasprite XPos of sprite in DrawSprite args
        sta DrawMetaSprite_XPos

        lda ActorsArray+Actor::YPos,X   ; store metasprite YPos in DrawSprite args
        sta DrawMetaSprite_YPos

        lda #$50                        ; store metasprite TileNum in DrawSprite args
        sta DrawMetaSprite_TileNum

        lda #$20                        ; store metasprite attributes in DrawSprite args
        sta DrawMetaSprite_Attribs

        lda #1                          ; store metasprite's # of tiles in DrawSprite args
        sta DrawMetaSprite_TotalTiles

        jsr DrawMetaSprite              ; call DrawMetaSprite using previously-assigned args
        jmp NextActor                   ; go to next actor
  :
    cmp #ActorType::PLAYER              ; is the actor a player?
    bne :+                              ; if no, skip; else, proceed
      RenderPlayer:
        lda ActorsArray+Actor::XPos,X   ; store metasprite XPos of sprite in DrawSprite args
        sta DrawMetaSprite_XPos

        lda ActorsArray+Actor::YPos,X   ; store metasprite YPos in DrawSprite args
        sta DrawMetaSprite_YPos

        lda #$60                        ; store metasprite TileNum in DrawSprite args
        sta DrawMetaSprite_TileNum

        lda #$00                        ; store metasprite attributes in DrawSprite args
        sta DrawMetaSprite_Attribs

        lda #4                          ; store metasprite's # of tiles in DrawSprite args
        sta DrawMetaSprite_TotalTiles

        jsr DrawMetaSprite              ; call DrawMetaSprite using previously-assigned args
        jmp NextActor                   ; go to next actor
  :
  cmp #ActorType::SUBMARINE             ; is the actor a missile?
    bne :+                              ; if no, skip; else, proceed
      RenderSubmarine:
        lda ActorsArray+Actor::XPos,X   ; store metasprite XPos of sprite in DrawSprite args
        sta DrawMetaSprite_XPos

        lda ActorsArray+Actor::YPos,X   ; store metasprite YPos in DrawSprite args
        sta DrawMetaSprite_YPos

        lda #$00                        ; store metasprite TileNum in DrawSprite args
        sta DrawMetaSprite_TileNum

        lda #$00                        ; store metasprite attributes in DrawSprite args
        sta DrawMetaSprite_Attribs

        lda #4                          ; store metasprite's # of tiles in DrawSprite args
        sta DrawMetaSprite_TotalTiles

        jsr DrawMetaSprite              ; call DrawMetaSprite using previously-assigned args
        jmp NextActor                   ; go to next actor
  :
  cmp #ActorType::MISSILE               ; is the actor a submarine?
    bne :+                              ; if no, skip; else, proceed
      RenderMissile:
        lda ActorsArray+Actor::XPos,X   ; store metasprite XPos of sprite in DrawSprite args
        sta DrawMetaSprite_XPos

        lda ActorsArray+Actor::YPos,X   ; store metasprite YPos in DrawSprite args
        sta DrawMetaSprite_YPos

        lda #$50                        ; store metasprite TileNum in DrawSprite args
        sta DrawMetaSprite_TileNum

        lda #$00                        ; store metasprite attributes in DrawSprite args
        sta DrawMetaSprite_Attribs

        lda #1                          ; store metasprite's # of tiles in DrawSprite args
        sta DrawMetaSprite_TotalTiles

        jsr DrawMetaSprite              ; call DrawMetaSprite using previously-assigned args
        jmp NextActor                   ; go to next actor
  :


NextActor:
    txa                                 ; increment loop index to refer to next actor (using actor size)
    clc
    adc #.sizeof(Actor)
    tax
    cmp #MAX_ACTORS * .sizeof(Actor)    ; have all actors been rendered?
    beq :+
        jmp ActorsLoop                  ; if no, repeat loop to render more actors
    :                                   ; (jmp is used here bc the destination is too far away for branching)

    rts                                 ; else, return from subroutine
.endproc