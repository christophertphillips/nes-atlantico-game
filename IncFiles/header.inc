;--------------------------------------------------------
; iNES header (contains a total of 16 bytes with the flags at $7FF0)
;--------------------------------------------------------
.segment "HEADER"
.byte $4E, $45, $53, $1A      ; 4 bytes with the characters 'N', 'E', 'S', "\n"
.byte $02                     ; how many 16kb chunks for PRG-ROM (=32kb)
.byte $02                     ; how many 8kb chunks for CHR-ROM (=16kb)
.byte $31                     ; vertical mirroring, mapper 003 (Mapper, mirroring, battery, trainer)
.byte $00                     ; nothing (Mapper, VS/Playchoice, NES 2.0)
.byte $00                     ; nothing (PRG-RAM)
.byte $00                     ; NTSC format
.byte $00                     ; nothing (TV system, PRG-RAM presence)
.byte $00, $00, $00, $00, $00  ; unused padding to complete 16 bytes of header