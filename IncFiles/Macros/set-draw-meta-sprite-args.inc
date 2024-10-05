.macro SET_DRAW_META_SPRITE_ARGS xpos, ypos, tilenum, attribs, totaltiles, screen
  lda xpos                              ; store metasprite XPos in DrawSprite args
  sta DrawMetaSprite_XPos

  lda ypos                              ; store metasprite YPos in DrawSprite args
  sta DrawMetaSprite_YPos

  lda tilenum                           ; store metasprite TileNum in DrawSprite args
  sta DrawMetaSprite_TileNum

  lda attribs                           ; store metasprite attributes in DrawSprite args
  sta DrawMetaSprite_Attribs

  lda totaltiles                        ; store metasprite's # of tiles in DrawSprite args
  sta DrawMetaSprite_TotalTiles

  lda screen                            ; store metasprite's first sprite screen value
  sta DrawMetaSprite_Screen
.endmacro
