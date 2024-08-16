.proc CheckActorCollision
  txa
  pha

  lda #0
  sta Collision

  ldx #0

ActorsLoop:
  lda ActorsArray+Actor::Type,x

  cmp #ActorType::AIRPLANE
  bne :+
    CheckAirplaneCollision:
      SET_CHECK_ACTOR_COLLISION_BOUNDS_ARGS #24, #8
      jsr CheckActorCollisionBounds

      lda Collision
      beq NextActor
          lda #ActorType::NULL
          sta ActorsArray+Actor::Type,X
          jmp EndCollisionCheck
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


EndCollisionCheck:

  pla
  tax

  rts                                 ; else, return from subroutine
.endproc
