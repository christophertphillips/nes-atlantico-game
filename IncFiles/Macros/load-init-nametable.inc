.macro LOAD_INIT_NAMETABLE

  lda #1                                ; temporarily set current CurNameTable to 1
  sta CurrNameTable

InitNameTableLoop:
      jsr LoadColumnTiles               ; load a new set of column tiles

    InitAttrBlockLoad:
      lda #$03                          ; is the column index a multiple of 4?
      bit SourceColIndex
      bne :+
          jsr LoadAttributeBlocks       ; if yes, load a new set of attribute block
:
      lda XScroll                       ; increment XScroll by 8 (to load next column on next iteration)
      clc
      adc #8
      sta XScroll

      inc SourceColIndex                ; increment source column index

      lda SourceColIndex                ; have all 32 initial columns been loaded?
      cmp #32
  bne InitNameTableLoop                 ; if no, perform another iteration

  ; lda #0
  ; sta XScroll

  lda #0                                ; revert CurNameTable back to 0
  sta CurrNameTable


.endmacro