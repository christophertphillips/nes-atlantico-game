.proc RenderScore
  STACK_PUSH X                          ; push UpdateActors loop index value to stack

  ldx #0                                ; index of buffer byte to store

StoreScoreBufferLength:
  lda #3                                ; store length of buffer entry (3) and increment index
  sta BUFFER_ADDR,X
  inx

StoreScoreBufferAddr:
  lda #$20                              ; store upper byte of score's nametable address and increment index
  sta BUFFER_ADDR,X                     ; (note that it's most significant byte first due to PPU latch order)
  inx

  lda #$52                              ; store lower byte of score's nametable address and increment index
  sta BUFFER_ADDR,X                     ; (note that it's most significant byte first due to PPU latch order)
  inx

StoreScoreBufferDigits:
  lda Score+2                           ; store hundreds digit and increment index
  clc
  adc #$60                              ; add offset for digits in pattern table
  sta BUFFER_ADDR,X
  inx

  lda Score+1                           ; store tens digit and increment index
  clc
  adc #$60                              ; add offset for digits in pattern table
  sta BUFFER_ADDR,X
  inx

  lda Score+0                           ; store ones digit and increment index
  clc
  adc #$60                              ; add offset for digits in pattern table
  sta BUFFER_ADDR,X
  ; inx

  STACK_PULL X                          ; pull UpdateActors loop index value from stack

  rts
.endproc
