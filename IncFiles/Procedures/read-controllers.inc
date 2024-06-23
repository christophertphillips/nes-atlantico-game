.proc ReadControllers
  lda #1                                ; set rightmost of Buttons to 1; use later to determine when Buttons is 'full'
  sta Buttons
  sta JOYPAD_1                          ; strobe latch to 'input mode' via Latch line
  lda #0
  sta JOYPAD_1                          ; strobe latch to 'output mode' via Latch line
Loop:
  lda JOYPAD_1                          ; 1) read bit from Data line and invert its
                                        ; 2) send signal via Clock line to shift bits inside controller
  lsr                                   ; right-shift accumualtor to put the just-read bit into the carry flag
  rol Buttons                           ; rotate-left Buttons to put the carry flag into the rightmost Buttons bit; initial 1 bit moves rightward
  bcc Loop                              ; if initial 1 bit has not reached the carry due to rol, get next controller bit

  rts                                   ; else, return from subroutine
.endproc