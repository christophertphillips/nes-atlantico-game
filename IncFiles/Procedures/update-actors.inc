.proc UpdateActors
  ldx #0                              ; index to reference actors

ActorsLoop:
  lda ActorsArray+Actor::Type,X       ; load type of currently-referenced actor

  cmp #ActorType::SPRITE0             ; is the actor sprite0?
  bne :+                              ; if no, skip; else, proceed
    UpdateSprite0:
  :
  cmp #ActorType::PLAYER              ; is the actor a player?
  bne :+                              ; if no, skip; else, proceed
    UpdatePlayer:
  :
  cmp #ActorType::SUBMARINE             ; is the actor a missile?
  bne :+                              ; if no, skip; else, proceed
    UpdateSubmarine:
  :
  cmp #ActorType::MISSILE             ; is the actor a submarine?
  bne :+                              ; if no, skip; else, proceed
    UpdateMissile:
      lda ActorsArray+Actor::YVel,X   ; load missile's YVel
      clc
      adc ActorsArray+Actor::YPos,X   ; add it to missile's YPos
      sta ActorsArray+Actor::YPos,X   ; store new missile YPos
  :

NextActor:
  txa                                 ; increment loop index to refer to next actor (using actor size)
  clc
  adc #.sizeof(Actor)
  tax
  cmp #MAX_ACTORS * .sizeof(Actor)    ; have all actors been updated?
  beq :+
      jmp ActorsLoop                  ; if no, repeat loop to update more actors
  :                                   ; (jmp is used here bc the destination is too far away for branching)

  rts                                 ; else, return from subroutine
.endproc
