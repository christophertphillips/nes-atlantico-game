.proc DrawMetaSprite
    ; DrawMetaSprite_XPos,
    ; DrawMetaSprite_YPos,
    ; DrawMetaSprite_TileNum,
    ; DrawMetaSprite_Attribs,
    ; DrawMetaSprite_TotalTiles,
    ; (OAMRAMIndex)

  txa
  pha

  ldx #0
  ldy OAMRAMIndex
SpriteLoop:
  lda DrawMetaSprite_YPos
  sta $0200,Y
  iny

  lda DrawMetaSprite_TileNum
  sta $0200,Y
  inc DrawMetaSprite_TileNum
  iny

  lda DrawMetaSprite_Attribs
  sta $0200,Y
  iny

  lda DrawMetaSprite_XPos
  sta $0200,Y
  clc
  adc #8
  sta DrawMetaSprite_XPos
  iny

  inx
  cpx DrawMetaSprite_TotalTiles
  bne SpriteLoop

  sty OAMRAMIndex

  pla
  tax

  rts
.endproc