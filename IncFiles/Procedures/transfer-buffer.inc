.proc TransferBuffer
  ldx #0                                ; index of buffer byte to transfer

BufferSegmentLoop:
  lda BUFFER_ADDR,X                     ; get length of buffer segment
  beq EndBufferSegmentLoop              ; is length = 0? if not, proceed to process segment

      tay                               ; A -> Y, where Y is a counter of data bytes remaining to transfer in segment

      inx                               ; increment buffer byte index

    SetBufferSegmentAddr:
      lda #%00000000                    ; increment PPU_DATA writes by 1
      sta PPU_CTRL

      bit PPU_STATUS                    ; reset PPU_ADDR latch
      lda BUFFER_ADDR,X                 ; sen`d upper byte of nametable address to PPU and increment index
      sta PPU_ADDR                      ; (note that it's most significant byte first due to PPU latch order)
      inx

      lda BUFFER_ADDR,X                 ; send lower byte of nametable address to PPU and increment index
      sta PPU_ADDR                      ; (note that it's most significant byte first due to PPU latch order)
      inx

    BufferDataLoop:
      lda BUFFER_ADDR,X                 ; send data byte to PPU and increment index
      sta PPU_DATA
      inx

      dey                               ; decrement counter of remaining bytes to transfer in segment
      bne BufferDataLoop                ; are there any more data bytes to tranfer in segment? if so, repeat

EndBufferSegmentLoop:
  rts
.endproc
