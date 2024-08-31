.proc LoadTitleScreen
  lda #<TitleScreenNameTable  ; load lower byte of TitleScreenNameTable address
  sta BgPtr
  lda #>TitleScreenNameTable  ; load upper byte of TitleScreenNameTable address
  sta BgPtr+1

  PPU_SETADDR $2000           ; set PPU address to $2000

  ldx #0                      ; init counters for hi/lo byte indexes
  ldy #0
OuterLoop:
InnerLoop:                    ; loop through all 256 bytes of current background data "chunk"
  lda (BgPtr),Y               ; lookup byte in ROM
  sta PPU_DATA                ; send value to PPU_DATA
  iny
  ; cpx #0                    ; check if current 256 byte "chunk" is completely loaded
  bne InnerLoop               ; if no, perform inner loop again
IncreaseHiByte:               ; else, increase hi byte of background pointer
  inc BgPtr+1
  inx
  cpx #4                      ; check if *all* background data has been loaded
  bne OuterLoop               ; if no, perform outer loop again

  rts                         ; else, return from subroutine
.endproc
