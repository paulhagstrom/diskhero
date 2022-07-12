; DISKHERO
; Apple III medium res display
;
; currently not doing anything
; occupies lines B0-BF at the bottom of the screen

; initialize with some kind of color pattern

MedGroups:  .byte   0
MedLines:   .byte   0
MedLine:    .byte   0
StartPix:   .byte   0

initmedres: lda #$01            ; number of groups
            sta MedGroups
            lda #$B0            ; first line
            sta MedLine
medresgrp:  lda #$07            ; number of lines to fill
            sta MedLines
            lda #%01010101
            sta StartPix
medresline: ldy MedLine
            lda YHiresL, y
            sta ZPtrA
            sta ZPtrB
            lda YHiresHA, y
            sta ZPtrA + 1
            lda YHiresHB, y
            sta ZPtrB + 1
            lda #$8F
            sta ZPtrA + XByte
            sta ZPtrB + XByte
            ldy #$27
            ldx #%00101001      ; color pattern to rotate
            lda StartPix        ; pixel pattern
medresbyte: sta (ZPtrA), y
            ror
            pha
            txa
            ror
            tax
            sta (ZPtrB), y
            pla
            dey
            bpl medresbyte
            inc MedLine
            ror StartPix
            dec MedLines
            bpl medresline
            dec MedGroups
            bpl medresgrp
            rts