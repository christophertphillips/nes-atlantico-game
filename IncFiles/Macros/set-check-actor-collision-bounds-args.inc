.macro SET_CHECK_ACTOR_COLLISION_BOUNDS_ARGS x1_offset, y1_offset
  lda ActorsArray+Actor::XPos,X
  sta CheckActorCollisionBounds_X0

  lda ActorsArray+Actor::YPos,X
  sta CheckActorCollisionBounds_Y0

  lda ActorsArray+Actor::XPos,X
  clc
  adc x1_offset
  sta CheckActorCollisionBounds_X1

  lda ActorsArray+Actor::YPos,X
  clc
  adc y1_offset
  sta CheckActorCollisionBounds_Y1
.endmacro
