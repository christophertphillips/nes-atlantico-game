.proc UpdateActors
  ldx #0                              ; index to reference actors

ActorsLoop:
  lda ActorsArray+Actor::Type,X       ; load type of currently-referenced actor

  ; cmp #ActorType::SPRITE0             ; is the actor sprite0?
  ; bne :+                              ; if no, skip; else, proceed
  ;   UpdateSprite0:
  ; :
  ; cmp #ActorType::PLAYER              ; is the actor a player?
  ; bne :+                              ; if no, skip; else, proceed
  ;   UpdatePlayer:
  ; :
  cmp #ActorType::SUBMARINE             ; is the actor a missile?
  bne :++                             ; if no, skip; else, proceed
    UpdateSubmarine:
      lda ActorsArray+Actor::XVel,X   ; load submarine's XVel
      clc
      adc ActorsArray+Actor::XPos,X   ; add it to submarien's XPos
      sta ActorsArray+Actor::XPos,X   ; store new submarine XPos

      bne :+                          ; has the submarine reached the left edge of the screen?
          lda #ActorType::NULL        ; if so, set actor type to NULL
          sta ActorsArray+Actor::Type,X
      :

      jmp NextActor
  :
  cmp #ActorType::AIRPLANE              ; is the actor an airplane?
  bne :++                               ; if no, skip; else, proceed
    UpdateAirplane:
      lda ActorsArray+Actor::XVel,X     ; load airplane's XVel
      clc
      adc ActorsArray+Actor::XPos,X     ; add it to airplane's XPos
      sta ActorsArray+Actor::XPos,X     ; store new airplane XPos

      bne :+                            ; has the airplane reached the left edge of the screen?
          lda #ActorType::NULL          ; if so, set actor type to NULL
          sta ActorsArray+Actor::Type,X
      :

      jmp NextActor
  :
  cmp #ActorType::MISSILE             ; is the actor a submarine?
  bne :++                             ; if no, skip; else, proceed
    UpdateMissile:
      lda ActorsArray+Actor::YVel,X   ; load missile's YVel
      clc
      adc ActorsArray+Actor::YPos,X   ; add it to missile's YPos
      sta ActorsArray+Actor::YPos,X   ; store new missile YPos

      bne :+                          ; has the missile reached the top of the screen?
          lda #ActorType::NULL        ; if so, set actor type to NULL
          sta ActorsArray+Actor::Type,X
          jmp NextActor               ; skip to next actor
      :

      lda ActorsArray+Actor::XPos,X
      clc
      adc #3
      sta CheckActorCollision_XPos

      lda ActorsArray+Actor::YPos,X
      clc
      adc #1
      sta CheckActorCollision_YPos

      jsr CheckActorCollision

      lda Collision
      beq NextActor
          lda #ActorType::NULL
          sta ActorsArray+Actor::Type,X
          ; jmp NextActor

      ; jmp NextActor
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
