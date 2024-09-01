.macro SET_SWITCH_CHR_BANK_ARGS bank
  lda bank                              ; set the bank to switch to
  sta SwitchCHRBank_Bank
.endmacro
