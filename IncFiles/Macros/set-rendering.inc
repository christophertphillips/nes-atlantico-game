.macro SET_RENDERING ctrl_val, mask_val
  lda ctrl_val
  sta PPU_CTRL
  lda mask_val
  sta PPU_MASK
.endmacro
