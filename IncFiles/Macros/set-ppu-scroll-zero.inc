.macro SET_PPU_SCROLL_ZERO ppu_ctr_val
  lda ppu_ctr_val                       ; set PPU_CTRL register
  sta PPU_CTRL
  lda #$00                              ; set PPU_SCROLL's X,Y values to 0
  sta PPU_SCROLL                        ; X
  sta PPU_SCROLL                        ; Y
.endmacro