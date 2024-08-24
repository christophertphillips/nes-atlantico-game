.proc IncrementScore
Increment1sDigit:
  lda Score+0                           ; increment 1s digit
  clc
  adc #1
  sta Score+0

  cmp #$A                               ; did the 1s digit roll over? if yes, increment 10s digit
  bne EndScoreIncrement

Increment10sDigit:
  lda #0                                ; set 1s digit to 0 (since it rolled over)
  sta Score+0

  lda Score+1                           ; increment 10s digit
  clc
  adc #1
  sta Score+1

  cmp #$A                               ; did the 10s digit roll over? if yes, increment 100s digit
  bne EndScoreIncrement

Increment100sDigit:
  lda #0                                ; set 10s digit to 0 (since it rolled over)
  sta Score+1

  lda Score+2                           ; increment 100s digit
  clc
  adc #1
  sta Score+2

  cmp #$A                               ; did the 100s digit roll over? if yes, increment 1000s digit
  bne EndScoreIncrement

Increment1000sDigit:
  lda #0                                ; set 100s digit to 0 (since it rolled over)
  sta Score+2

  lda Score+3                           ; increment 1000s digit
  clc
  adc #1
  sta Score+3

EndScoreIncrement:
  rts
.endproc
