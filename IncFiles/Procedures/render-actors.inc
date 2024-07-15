.proc RenderActors
  ldx #0
  lda #20
  sta OAMRAMIndex

  ActorsLoop:
    lda ActorsArray+Actor::Type,X

    cmp #ActorType::PLAYER
    bne :+
        lda ActorsArray+Actor::XPos,X
        sta DrawMetaSprite_XPos

        lda ActorsArray+Actor::YPos,X
        sta DrawMetaSprite_YPos

        lda #$60
        sta DrawMetaSprite_TileNum

        lda #$00
        sta DrawMetaSprite_Attribs

        lda #4
        sta DrawMetaSprite_TotalTiles

        jsr DrawMetaSprite
  :

  NextActor:
    txa
    clc
    adc #.sizeof(Actor)
    tax
    cmp #MAX_ACTORS * .sizeof(Actor)
    bne ActorsLoop

    rts
.endproc