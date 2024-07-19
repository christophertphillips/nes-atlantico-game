.proc DrawMetaSprite
    ; DrawMetaSprite_XPos,
    ; DrawMetaSprite_YPos,
    ; DrawMetaSprite_TileNum,
    ; DrawMetaSprite_Attribs,
    ; DrawMetaSprite_TotalTiles,
    ; (OAMRAMIndex)

  txa                                   ; push X onto stack
  pha

  ldx #0
  ldy OAMRAMIndex
SpriteLoop:
  lda DrawMetaSprite_YPos               ; store tile's YPos in OAMRAM
  sta $0200,Y
  iny

  lda DrawMetaSprite_TileNum            ; store tile's TileNum in OAMRAM
  sta $0200,Y
  inc DrawMetaSprite_TileNum            ; increment TileNum to properly identify next metasprite tile in pattern table
  iny

  lda DrawMetaSprite_Attribs            ; store tile's attributes in OAMRAM
  sta $0200,Y
  iny

  lda DrawMetaSprite_XPos               ; store tile's XPos in OAMRAM
  sta $0200,Y
  clc                                   ; increment XPos by 8 to properly position next metasprite tile
  adc #8
  sta DrawMetaSprite_XPos               ; store new XPos for next metasprite tile
  iny

  inx
  cpx DrawMetaSprite_TotalTiles         ; have all metasprite tiles been rendered?
  bne SpriteLoop                        ; if no, repeat loop to render more tiles; else, continue

  sty OAMRAMIndex

  pla                                   ; pull X from stack
  tax

  rts                                   ; return from subroutine
.endproc