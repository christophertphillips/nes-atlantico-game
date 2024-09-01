.macro POLL_IS_NMI_COMPLETE top_loop_addr
PollIsNMIComplete:
  lda IsNMIComplete                     ; wait for IsNMIComplete to be set (=1)
  beq PollIsNMIComplete
ResetIsNMIComplete:
  lda #0                                ; reset IsNMIComplete flag
  sta IsNMIComplete
  jmp top_loop_addr                     ; return to top of loop
.endmacro
