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
  bne :+                              ; if no, skip; else, proceed
    UpdateSubmarine:
      lda ActorsArray+Actor::XVel,X   ; load submarine's XVel
      clc
      adc ActorsArray+Actor::XPos,X   ; add it to submarien's XPos
      sta ActorsArray+Actor::XPos,X   ; store new submarine XPos

      lda ActorsArray+Actor::Screen,X   ; set the carry for the first metasprite tile of the actor using the carry from the prior addition
      sbc #0                            ; subtract 0, since the carry is the only quantity of interest
      sta ActorsArray+Actor::Screen,X

      cmp #$FF                          ; is the submarine on screen -1?
      bne EmdSubmarineUpdate            ; if no, skip
          lda ActorsArray+Actor::XPos,X ; else, is the submarine less than 224 ($E0) on screen -1?
          cmp #$E0
          bcs EmdSubmarineUpdate        ; if no, skip
              lda #ActorType::NULL      ; else, set actor type to NULL
              sta ActorsArray+Actor::Type,X

    EmdSubmarineUpdate:
      jmp NextActor
  :
  cmp #ActorType::AIRPLANE              ; is the actor an airplane?
  bne :+                                ; if no, skip; else, proceed
    UpdateAirplane:
      lda ActorsArray+Actor::XVel,X     ; load airplane's XVel
      clc
      adc ActorsArray+Actor::XPos,X     ; add it to airplane's XPos
      sta ActorsArray+Actor::XPos,X     ; store new airplane XPos

      lda ActorsArray+Actor::Screen,X   ; set the carry for the first metasprite tile of the actor using the carry from the prior addition
      sbc #0                            ; subtract 0, since the carry is the only quantity of interest
      sta ActorsArray+Actor::Screen,X

      cmp #$FF                          ; is the airplane on screen -1?
      bne EndAirplaneUpdate             ; if no, skip
          lda ActorsArray+Actor::XPos,X ; else, is the airplane less than 232 ($E8) on screen -1?
          cmp #$E8
          bcs EndAirplaneUpdate         ; if no, skip
              lda #ActorType::NULL      ; else, set actor type to NULL
              sta ActorsArray+Actor::Type,X

    EndAirplaneUpdate:
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

      SET_CHECK_ACTOR_COLLISION_ARGS #3, #1 ; check for collision b/t missile and airplanes
      jsr CheckActorCollision

      lda Collision
      beq NextActor
          lda #ActorType::NULL
          sta ActorsArray+Actor::Type,X

          jsr IncrementScore          ; increment Score RAM var
          jsr RenderScore             ; copy updated score to nametable buffer
          ; jmp NextActor

          STACK_PUSH X                ; push UpdateActors loop index value to stack
          lda #1                      ; play explosion sound effect
          ldx #FAMISTUDIO_SFX_CH1
          jsr famistudio_sfx_play
          STACK_PULL X                ; pull UpdateActors loop index value from stack

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
