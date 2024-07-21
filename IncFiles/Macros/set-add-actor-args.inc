.macro SET_ADD_ACTOR_ARGS type, xpos, ypos, xvel, yvel
  lda type                              ; set the Type argument for AddActor
  sta AddActor_Type
  lda xpos                              ; set the XPos argument for AddActor
  sta AddActor_XPos
  lda ypos                              ; set the YPos argument for AddActor
  sta AddActor_YPos
  lda xvel                              ; set the XVel argument for AddActor
  sta AddActor_XVel
  lda yvel                              ; set the YVel argument for AddActor
  sta AddActor_YVel
.endmacro