.proc SetTitleScreenCursorPosition
  lda #$5E                              ; set initial cursor position

  ldx TitleScreenSelection              ; get cursor selection index
  beq :+                                ; if cursor selection index = 0, skip
    CursorOffsetLoop:                   ; else, begin looping...
      clc                               ; add offset to cursor's y-position
      adc #$10

      dex                               ; decrement looping index
      bne CursorOffsetLoop              ; is looping index = 0?
  :

  sta OAM_RAM_ADDR+0                    ; set y-position of cursor

  rts                                   ; return from subroutine
.endproc
