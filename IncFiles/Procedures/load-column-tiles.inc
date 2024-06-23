.proc LoadColumnTiles
  ; calculate destination column address
CalculateDestColAddrLoByte:
  lda XScroll                           ; divide current XScroll value by 8
  lsr
  lsr
  lsr
  sta DestAddr                          ; set the lo byte of the destination column address ($00, $01, $02, ..., $1E, $1F)

CalculateDestColAddrHiByte:
  lda CurrNameTable                     ; get current NameTable value (0 or 1) and multiply by 4
  eor #1
  asl
  asl
  clc
  adc #$20                              ; add $20 (resulting in $20 or $24) for NameTable 0 or 1
  sta DestAddr+1                        ; set the hi byte of the destination column address ($20XX or $24XX)

  ; calculate source column address
CalculateSourceColAddrLoByte:
  lda SourceColIndex                    ; mutiply current source column index by 32
  asl
  asl
  asl
  asl
  asl
  sta SourceAddr                        ; set the lo byte of the source column address ($00, $20, $40, $60, $80, $A0, $C0, $E0)

CalculateSourceColAddrHiByte:
  lda SourceColIndex                    ; divide the current source column index by 8
  lsr
  lsr
  lsr
  sta SourceAddr+1                      ; set the hi byte of the source column ($00XX, $01XX, $02XX, ...)

  ; add BackgroundData offset to source column address
AddOffsetSourceColAddrLoByte:
  lda SourceAddr                        ; add lo byte of BackgroundData offset to lo byte of SourceAddr
  clc
  adc #<BackgroundData
  sta SourceAddr                        ; set the (offsetted) lo byte of the source column address

AddOffsetSourceColAddrHiByte:
  lda SourceAddr+1                      ; add hi byte of BackgroundData offset to hi byte of SourceAddr
  adc #>BackgroundData
  sta SourceAddr+1                      ; set the (offsetted) hi byte of the source column address

SetPPUColTiles:
  lda #%00000100                        ; increment PPU_DATA writes by 32
  sta PPU_CTRL

  bit PPU_STATUS                        ; reset PPU_ADDR latch
  lda DestAddr+1                        ; send hi byte of DestAddr to PPU_ADDR
  sta PPU_ADDR
  lda DestAddr                          ; send lo byte of DestAddr to PPU_ADDR
  sta PPU_ADDR

  ldy #0                                ; initialize index register value
  ldx #30
SetPPUColTilesLoop:                     ; draw all 30 tiles (rows) of the current column
  lda (SourceAddr),Y                    ; get column tile from source column address + offset
  sta PPU_DATA                          ; send value to PPU_DATA
  iny                                   ; increment index register value
  dex
  bne SetPPUColTilesLoop                ; have all 30 tiles (rows) of the current column been drawn?

  rts
.endproc