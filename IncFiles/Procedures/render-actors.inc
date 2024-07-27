.proc RenderActors
  ldx #0                                ; index to reference actors
  lda #0                                ; reset OAM RAM index to zero (i.e. $0200 in RAM)
  sta OAMRAMIndex

ActorsLoop:
    lda ActorsArray+Actor::Type,X       ; load type of currently-referenced actor

    cmp #ActorType::SPRITE0             ; is the actor sprite0?
    bne :+                              ; if no, skip; else, proceed
      RenderSprite0:
        SET_DRAW_META_SPRITE_ARGS {ActorsArray+Actor::XPos,X}, {ActorsArray+Actor::YPos,X}, #$50, #$20, #1

        jsr DrawMetaSprite              ; call DrawMetaSprite using previously-assigned args
        jmp NextActor                   ; go to next actor
  :
    cmp #ActorType::PLAYER              ; is the actor a player?
    bne :+                              ; if no, skip; else, proceed
      RenderPlayer:
        SET_DRAW_META_SPRITE_ARGS {ActorsArray+Actor::XPos,X}, {ActorsArray+Actor::YPos,X}, #$60, #$00, #4

        jsr DrawMetaSprite              ; call DrawMetaSprite using previously-assigned args
        jmp NextActor                   ; go to next actor
  :
  cmp #ActorType::SUBMARINE             ; is the actor a missile?
    bne :+                              ; if no, skip; else, proceed
      RenderSubmarine:
        SET_DRAW_META_SPRITE_ARGS {ActorsArray+Actor::XPos,X}, {ActorsArray+Actor::YPos,X}, #$00, #$00, #4

        jsr DrawMetaSprite              ; call DrawMetaSprite using previously-assigned args
        jmp NextActor                   ; go to next actor
  :
  cmp #ActorType::MISSILE               ; is the actor a submarine?
    bne :+                              ; if no, skip; else, proceed
      RenderMissile:
        SET_DRAW_META_SPRITE_ARGS {ActorsArray+Actor::XPos,X}, {ActorsArray+Actor::YPos,X}, #$50, #$00, #1

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