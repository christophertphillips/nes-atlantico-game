.macro CHECK_BUTTON button_const
  lda button_const
  bit Buttons
.endmacro