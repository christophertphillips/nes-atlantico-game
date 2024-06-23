.proc LoadPalette
  PPU_SETADDR $3F00                     ; set PPU address to $3F00

 ldx #0
Loop:
  lda PaletteData,X                     ; get color value
  sta PPU_DATA                          ; send value to PPU_DATA
  inx
  cpx #32                               ; loop through all 32 colors
  bne Loop

  rts
.endproc