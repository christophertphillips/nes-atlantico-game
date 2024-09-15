.proc LoadTitleScreenRLE
  lda #<TitleScreenNameTable  ; load lower byte of TitleScreenNameTable address
  sta BgPtr
  lda #>TitleScreenNameTable  ; load upper byte of TitleScreenNameTable address
  sta BgPtr+1

  PPU_SETADDR $2000           ; set PPU address to $2000

  ldy #0                      ; init index for length/tile value pairs
LengthLoop:
  lda (BgPtr),Y               ; load length of run
  beq EndLoadTitleScreenRLE   ; if the length is 0, the endcoding is done; jump to end
  iny                         ; increment length index
  bne :+                      ; has the index rolled over?
      inc BgPtr+1             ; if so, increment hi byte of BgPtr
  :

  tax                         ; transfer length to X (tile counter)
  lda (BgPtr),Y               ; load tile value
  iny                         ; increment length index
  bne :+                      ; has the index rolled over?
      inc BgPtr+1             ; if so, increment hi byte of BgPtr
  :

TileLoop:
  sta PPU_DATA                ; send tile value to PPU_DATA
  dex                         ; decrement tile counter
  bne TileLoop                ; loop until tile counter = 0

  jmp LengthLoop              ; jump back to start of length loop

EndLoadTitleScreenRLE:
  rts                         ; else, return from subroutine
.endproc
