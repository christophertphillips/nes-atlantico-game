.proc LoadSprites
  ldx #0
Loop:
  lda SpriteData,X                      ; load tile
  sta $0200,X                           ; send tile to RAM (to be transferred to VRAM via DMA later)
  inx
  cpx #20                               ; check if all tiles have been send to RAM
  bne Loop                              ; if no, loop and store next tile

  rts                                   ; else, return from subroutine
.endproc