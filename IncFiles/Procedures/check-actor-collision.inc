.proc CheckActorCollision
  txa                                   ; push X to stack
  pha

  lda #0                                ; reset collision indicator variable
  sta Collision

  ldx #0                                ; index to reference actors
ActorsLoop:
  lda ActorsArray+Actor::Type,X         ; load type of currently-referenced actor

  cmp #ActorType::AIRPLANE              ; is the actor an airplane?
  bne :+                                ; if no, skip; else, proceed
    CheckAirplaneCollision:
      SET_CHECK_ACTOR_COLLISION_BOUNDS_ARGS #24, #8 ; check if source actor collided with the airplane
      jsr CheckActorCollisionBounds

      lda Collision                     ; did a collision occur?
      beq NextActor                     ; if no, skip; else, proceed
          lda #ActorType::NULL          ; set actor type to NULL
          sta ActorsArray+Actor::Type,X
          jmp EndCollisionCheck         ; break out of ActorsLoop (no need to continue checking collisions)
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
