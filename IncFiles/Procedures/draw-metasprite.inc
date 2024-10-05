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
  lda DrawMetaSprite_Screen             ; is the metasprite tile in screen 0?
  bne SkipSprite                        ; if no, skip to the "SkipSprite" code block

  lda DrawMetaSprite_YPos               ; store tile's YPos in OAMRAM
  sta OAM_RAM_ADDR,Y
  iny

  lda DrawMetaSprite_TileNum            ; store tile's TileNum in OAMRAM
  sta OAM_RAM_ADDR,Y
  inc DrawMetaSprite_TileNum            ; increment TileNum to properly identify next metasprite tile in pattern table
  iny

  lda DrawMetaSprite_Attribs            ; store tile's attributes in OAMRAM
  sta OAM_RAM_ADDR,Y
  iny

  lda DrawMetaSprite_XPos               ; store tile's XPos in OAMRAM
  sta OAM_RAM_ADDR,Y
  clc                                   ; increment XPos by 8 to properly position next metasprite tile
  adc #8
  sta DrawMetaSprite_XPos               ; store new XPos for next metasprite tile
  iny

  lda DrawMetaSprite_Screen             ; set the screen value for the next metasprite tile using the carry from the prior addition
  adc #0                                ; add 0, since the carry is the only quantity of interest
  sta DrawMetaSprite_Screen             ; (if the prior addition > 256, move one screen to the right)

  jmp NextSprite                        ; skip the "SkipSprite" code block and go to next metasprite tile

SkipSprite:
  lda DrawMetaSprite_XPos               ; store tile's XPos in OAMRAM
  clc                                   ; increment XPos by 8 to properly position next metasprite tile
  adc #8
  sta DrawMetaSprite_XPos               ; store new XPos for next metasprite tile

  lda DrawMetaSprite_Screen             ; set the screen value for the next metasprite tile using the carry from the prior addition
  adc #0                                ; add 0, since the carry is the only quantity of interest
  sta DrawMetaSprite_Screen             ; (if the prior addition > 256, move one screen to the right)

  inc DrawMetaSprite_TileNum            ; increment TileNum to properly identify next metasprite tile in pattern table

NextSprite:
  inx
  cpx DrawMetaSprite_TotalTiles         ; have all metasprite tiles been rendered?
  bne SpriteLoop                        ; if no, repeat loop to render more tiles; else, continue

  sty OAMRAMIndex

  pla                                   ; pull X from stack
  tax

  rts                                   ; return from subroutine
.endproc