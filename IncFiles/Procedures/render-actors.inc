.proc RenderActors
  ldx #0                                ; index to reference actors
  lda #0                                ; reset OAM RAM index to zero (i.e. $0200 in RAM)
  sta OAMRAMIndex

ActorsLoop:
    lda ActorsArray+Actor::Type,X       ; load type of currently-referenced actor

    cmp #ActorType::SPRITE0             ; is the actor sprite0?
    bne :+                              ; if no, skip; else, proceed
      RenderSprite0:
        SET_DRAW_META_SPRITE_ARGS {ActorsArray+Actor::XPos,X}, {ActorsArray+Actor::YPos,X}, #$50, #$20, #1, {ActorsArray+Actor::Screen,X}

        jsr DrawMetaSprite              ; call DrawMetaSprite using previously-assigned args
        jmp NextActor                   ; go to next actor
  :
    cmp #ActorType::PLAYER              ; is the actor a player?
    bne :+                              ; if no, skip; else, proceed
      RenderPlayer:
        SET_DRAW_META_SPRITE_ARGS {ActorsArray+Actor::XPos,X}, {ActorsArray+Actor::YPos,X}, #$60, #$00, #4, {ActorsArray+Actor::Screen,X}

        jsr DrawMetaSprite              ; call DrawMetaSprite using previously-assigned args
        jmp NextActor                   ; go to next actor
  :
  cmp #ActorType::SUBMARINE             ; is the actor a missile?
    bne :+                              ; if no, skip; else, proceed
      RenderSubmarine:
        SET_DRAW_META_SPRITE_ARGS {ActorsArray+Actor::XPos,X}, {ActorsArray+Actor::YPos,X}, #$00, #$00, #4, {ActorsArray+Actor::Screen,X}

        jsr DrawMetaSprite              ; call DrawMetaSprite using previously-assigned args
        jmp NextActor                   ; go to next actor
  :
  cmp #ActorType::AIRPLANE               ; is the actor an airplane?
    bne :+                              ; if no, skip; else, proceed
      RenderAirplane:
        SET_DRAW_META_SPRITE_ARGS {ActorsArray+Actor::XPos,X}, {ActorsArray+Actor::YPos,X}, #$10, #$03, #3, {ActorsArray+Actor::Screen,X}

        jsr DrawMetaSprite              ; call DrawMetaSprite using previously-assigned args
        jmp NextActor                   ; go to next actor
  :
  cmp #ActorType::MISSILE               ; is the actor a submarine?
    bne :+                              ; if no, skip; else, proceed
      RenderMissile:
        SET_DRAW_META_SPRITE_ARGS {ActorsArray+Actor::XPos,X}, {ActorsArray+Actor::YPos,X}, #$50, #$00, #1, {ActorsArray+Actor::Screen,X}

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

ClearStaleOAMRAM:
  ldy OAMRAMIndex
ClearStaleOAMRAMDataLoop:
  cpy PrevOAMRAMIndex                   ; is there "stale" data in OAM RAM?
  bcs :+                                ; if no, skip
      lda #0                            ; else, zero out the stale data

      sta OAM_RAM_ADDR,Y                ; (performed in 4-byte chunks, since each hardware sprite corresponds to 4 bytes of OAM RAM)
      iny
      sta OAM_RAM_ADDR,Y
      iny
      sta OAM_RAM_ADDR,Y
      iny
      sta OAM_RAM_ADDR,Y
      iny

      jmp ClearStaleOAMRAMDataLoop
    :

    lda OAMRAMIndex                     ; store current OAM RAM index in previous OAM RAM index, for next render
    sta PrevOAMRAMIndex

    rts                                 ; else, return from subroutine
.endproc
