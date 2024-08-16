.macro SET_CHECK_ACTOR_COLLISION_ARGS x_offset, y_offset
  lda ActorsArray+Actor::XPos,X
  clc
  adc x_offset
  sta CheckActorCollision_XPos

  lda ActorsArray+Actor::YPos,X
  clc
  adc y_offset
  sta CheckActorCollision_YPos
.endmacro
