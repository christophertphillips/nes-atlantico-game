.proc AddActor
ldx #0                                  ; index to reference ActorsArray positions

ActorArrayLoop:
  cpx #MAX_ACTORS * .sizeof(Actor)      ; is the ActorsArray full?
  beq EndRoutine                        ; if yes, jump to end of routine; else, proceed

  lda ActorsArray+Actor::Type,X         ; load type at current position in ActorsArray
  cmp #ActorType::NULL                  ; is the type at the current position NULL?
  beq AddActorToActorArray              ; if yes, proceed to AddActorToArray; else, increase actor index

IncreaseActorArrayLoopIndex:
  txa                                   ; increment loop index to refer to next actor index (using actor size)
  clc
  adc #.sizeof(Actor)
  tax
  jmp ActorArrayLoop                    ; repeat loop at next actor index
  
AddActorToActorArray:
  lda AddActor_Type                     ; store actor type in ActorsArray entry
  sta ActorsArray+Actor::Type,X

  lda AddActor_XPos                     ; store actor XPos in ActorsArray entry
  sta ActorsArray+Actor::XPos,X

  lda AddActor_YPos                     ; store actor YPos in ActorsArray entry
  sta ActorsArray+Actor::YPos,X

  lda AddActor_XVel                     ; store actor XVel in ActorsArray entry
  sta ActorsArray+Actor::XVel,X

  lda AddActor_YVel                     ; store actor YVel in ActorsArray entry
  sta ActorsArray+Actor::YVel,X

  lda #0                                ; store actor Screen in ActorsArray entry
  sta ActorsArray+Actor::Screen,X       ; (assume all actors start on screen 0)

EndRoutine:
  rts
.endproc