.proc LoadPalette
SetGamePalette:
  lda LoadPalette_Index                 ; load palette index arg
  cmp #TitleScreenSelection::CLEARDAY   ; is the clearday palette selected?
  bne :+                                ; if no, skip
      lda #<PaletteDataClearDay         ; else, point PalettePtr to clearday and jump to end
      sta PalettePtr+0
      lda #>PaletteDataClearDay
      sta PalettePtr+1
      jmp EndSetGamePalette
  :
  cmp #TitleScreenSelection::OVERCAST   ; is the overcast palette selected?
  bne :+                                ; if no, skip
      lda #<PaletteDataOvercast         ; else, point PalettePtr to overcast and jump to end
      sta PalettePtr+0
      lda #>PaletteDataOvercast
      sta PalettePtr+1
      jmp EndSetGamePalette
  :
  cmp #TitleScreenSelection::NIGHT      ; is the night palette selected?
  bne :+                                ; if no, skip
      lda #<PaletteDataNight            ; else, point PalettePtr to night and jump to end
      sta PalettePtr+0
      lda #>PaletteDataNight
      sta PalettePtr+1
      jmp EndSetGamePalette
  :
EndSetGamePalette:

  PPU_SETADDR $3F00                     ; set PPU address to $3F00

 ldy #0
Loop:
  lda (PalettePtr),Y                    ; get color value
  sta PPU_DATA                          ; send value to PPU_DATA
  iny
  cpy #32                               ; loop through all 32 colors
  bne Loop

  rts
.endproc