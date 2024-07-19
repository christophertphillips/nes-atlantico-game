.proc RenderActors
  ldx #0                                ; index to reference actors
  lda #20
  sta OAMRAMIndex

  ActorsLoop:
    lda ActorsArray+Actor::Type,X       ; load type of currently-referenced actor

    cmp #ActorType::SPRITE0             ; is the actor sprite0?
    bne :+                              ; if no, skip; else, proceed
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
  :
    cmp #ActorType::PLAYER              ; is the actor a player?
    bne :+                              ; if no, skip; else, proceed
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
  :

  NextActor:
    txa                                 ; increment loop index to refer to next actor (using actor size)
    clc
    adc #.sizeof(Actor)
    tax
    cmp #MAX_ACTORS * .sizeof(Actor)    ; have all actors been rendered?
    bne ActorsLoop                      ; if no, repeat loop to render more actors

    rts                                 ; else, return from subroutine
.endproc