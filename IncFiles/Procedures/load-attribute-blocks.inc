.proc LoadAttributeBlocks
  ; calculate destination attribute block address (requires adding 960 to the base, e.g. $23C0 or $27C0)
CalculateDestAttrBlockAddrLoByte:
  lda XScroll                           ; divide current XScroll value by 32
  lsr
  lsr
  lsr
  lsr
  lsr
  clc
  adc #$C0                              ; add $C0 (due to 960 offset)
  sta DestAddr                          ; set the lo byte of the destination attribute block address ($C0, $C1, $C2, ..., $C6, $C7)

  CalculateDestAttrBlockAddrHiByte:
  lda CurrNameTable                     ; get current NameTable value (0 or 1) and multiply by 4
  eor #1
  asl
  asl
  clc
  adc #$23                              ; add $23 (due to 960 offset) (resulting in $23 or $27) for NameTable 0 or 1
  sta DestAddr+1                        ; set the hi byte of the destination attribute block address ($23XX or $27XX)

; calculate source column address
CalculateSourceAttrBlockAddrLoByte:
  lda SourceColIndex                    ; calculate (SourceColIndex / 4) * 8 = (SourceColIndex's closest lowest multiple of 4) * 2
  and #$FC                              ; get closet lowest multiple of 4
  asl                                   ; multiply by 2
  sta SourceAddr                        ; set the lo byte of the source attribute block address ($00, $08, $10, ..., $F0, $F8)

CalculateSourceAttrBlockAddrHiByte:
  lda SourceColIndex                    ; divide the current source column index by 128
  lsr                                   ; (bc topmost bit must be incremented every 32 attribute tile columns, and
  lsr                                   ;  SourceColIndex increases by 4 for every attribute tile column,
  lsr                                   ;  thus 32 * 4 = 128)
  lsr
  lsr
  lsr
  lsr
  sta SourceAddr+1                      ; set the hi byte of the source attribute block address ($00XX, $01XX, $02XX, ...)

  ; add AttributeData offset to source column address
AddOffsetSourceAttrBlockAddrLoByte:
  lda SourceAddr                        ; add lo byte of BackgroundData offset to lo byte of SourceAddr
  clc
  adc #<AttributeData
  sta SourceAddr                        ; set the (offsetted) lo byte of the attribute block address

AddOffsetSourceAttrBlockAddrHiByte:
  lda SourceAddr+1                      ; add hi byte BackgroundData offset to lo byte of SourceAddr
  adc #>AttributeData
  sta SourceAddr+1                      ; set the (offsetted) hi byte of the attribute block address

  ; lda #%00000000                      ; no need to set PPU_CTRL's increment value, since we manually set PPU_ADDR every loop iteration
  ; sta PPU_CTRL

  ; send attribute block values to PPU
  ldy #0
SetPPUAttrBlocks:
  bit PPU_STATUS                        ; set PPU_ADDR to address for current attribute block
  lda DestAddr+1                        ; send hi byte of DestAddr to PPU_ADDR
  sta PPU_ADDR
  lda DestAddr                          ; send lo byte of DestAddr to PPU_ADDR
  sta PPU_ADDR

  lda (SourceAddr),Y                    ; get attribute block from source column address + offset
  sta PPU_DATA                          ; send value to PPU_DATA

  lda DestAddr                          ; increment DestAttrBlockAddr by 8
  clc                                   ; (to load next attribute block value in correct place)
  adc #8
  sta DestAddr

  iny                                   ; increment index register value
  cpy #8                                ; have all 8 attribute blocks (rows) of the current attribute block column been drawn?
  bne SetPPUAttrBlocks

  rts
.endproc