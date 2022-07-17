; DISKHERO
; Apple III super-hires region
;
; occupies  lines 00-0F (10)
;
; has no present function

; fill with a pattern

SHRGroups:  .byte   0
SHRLines:   .byte   0
SHRLine:    .byte   0
SHRStart:   .byte   0

initshgr:   lda #$00            ; number of groups
            sta SHRGroups
            lda #$00            ; first line
            sta SHRLine
shgrgrp:    lda #$0F            ; number of lines to fill
            sta SHRLines
            lda #%01010101
            sta SHRStart
shgrline:   ldy SHRLine
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
            lda SHRStart        ; pixel pattern
shgrbyte:   sta (ZPtrA), y
            ror
            pha
            txa
            ror
            tax
            sta (ZPtrB), y
            pla
            dey
            bpl shgrbyte
            inc SHRLine
            ror SHRStart
            dec SHRLines
            bpl shgrline
            dec MedGroups
            bpl shgrgrp
            rts