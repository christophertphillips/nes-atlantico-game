.proc SwitchCHRBank
  lda SwitchCHRBank_Bank                ; store desired CHR bank in bank register
  sta BANK_REGISTER

  rts                                   ; return from procedure
.endproc
