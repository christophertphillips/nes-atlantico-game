.macro STACK_PUSH register

    .if(.match({register}, X))
      txa
    .elseif(.match({register}, Y))
      tya
    .endif

    pha

.endmacro

.macro STACK_PULL register

    pla

    .if(.match({register}, X))
      tax
    .elseif(.match({register}, Y))
      tay
    .endif    

.endmacro

.macro STACK_PUSH_ALL

  pha                                   ; push A,X,Y registers to the stack
  txa
  pha
  tya
  pha

.endmacro

.macro STACK_PULL_ALL

  pla                                   ; pull A,X,Y registers from the stack
  tay
  pla
  tax
  pla

.endmacro