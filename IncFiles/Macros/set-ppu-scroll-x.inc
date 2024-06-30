.macro SET_PPU_SCROLL_X ppu_ctr_val  
  lda ppu_ctr_val                       
  ora CurrNameTable                     ; set base NameTable
  sta PPU_CTRL                          ; set PPU_CTRL register
  lda XScroll                           ; set PPU_SCROLL's X value to XScroll value
  sta PPU_SCROLL                        ; X
  lda #$00                              ; set PPU_SCROLL's Y value to 0
  sta PPU_SCROLL                        ; Y
.endmacro