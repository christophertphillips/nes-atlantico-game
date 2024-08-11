.proc GetRandomByte
  ldy #8                                ; index to loop 8 times (for 8 bits)
  lda Seed+0                            ; load the low byte of the 16-bit seed
RandomBitLoop:
  asl                                   ; shift low byte left, placing most significant bit in carry
  rol Seed+1                            ; rotate high byte of seed, placing carry bit in its least significant bit, and output bit in carry
  bcc :+                                ; was the output bit a 0? if yes, skip
    eor #$39                            ; else, XOR the carry bit (1) with the taps of the LSFR
  :
  dey                                   ; decrease index
  bne RandomBitLoop                     ; have all 8 bits been generated? if no, generate another

  sta Seed+0                            ; store the resulting random byte into the low byte of the seed (for use on next call of GetRandomByte)

  cmp #0                                ; set flags according to the random byte's properties (e.g. negative, zero, etc.)

  rts                                   ; return from procedure
.endproc
