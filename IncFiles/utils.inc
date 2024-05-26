.macro PPU_SETADDR addr
  
  bit PPU_STATUS              ; reset PPU_ADDR latch
  lda #>addr                  ; read hi-byte of addr
  sta PPU_ADDR                ; send hi-byte of addr to PPU_ADDR ($2006)
  lda #<addr                  ; read lo-byte of addr
  sta PPU_ADDR                ; send lo-byte of addr to PPU_ADDR ($2006)

.endmacro

.macro PPU_SETDATA val
  lda val
  sta PPU_DATA                ; send value to PPU_DATA ($2007)
.endmacro

.macro CHECK_BUTTON button_const
  lda button_const
  bit Buttons
.endmacro

.macro LOOP_BACKGROUND_DATA
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
.endmacro