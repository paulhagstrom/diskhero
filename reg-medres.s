; DISKHERO
; Apple III medium res display
;
; displays compasses toward closest instance of a disk type
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
            clc
            adc #$20            ; compute HB
            sta ZPtrB + 1
            lda #$8F
            sta ZPtrA + XByte
            sta ZPtrB + XByte
            ldy #$27
            ;ldx #%00101001      ; color pattern to rotate
            ldx #%00101000      ; color pattern to rotate
            lda StartPix        ; pixel pattern
medresbyte: sta (ZPtrA), y
            ror
            pha
            txa
            ;ror
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

MRColU:     .byte   0, 0, 0, 0
MRColD:     .byte   0, 0, 0, 0
MRColL:     .byte   0, 0, 0, 0
MRColR:     .byte   0, 0, 0, 0
MROffsetU:  .byte   0, 0, 0, 0
MROffsetD:  .byte   0, 0, 0, 0
MROffsetL:  .byte   0, 0, 0, 0
MROffsetR:  .byte   0, 0, 0, 0
MRLines:    .byte   0

FatHOffset  = $00
ThinHOffset = $10
FatVOffset  = $00
ThinVOffset = $10
BrightColor = $00   ; magic - zero = diskcolor for type
DimColor    = $52   ; non-magic - nonzero = use that color
BackColor   = $02   ; maybe-magic = background color for magic foreground
LeftArrow   = $20
RightArrow  = $00
UpArrow     = $40
DownArrow   = $60

; draw compass arrows for each of the four disk types
; showing where the closest one is relative to hero
; (Computed in domove)

drawmedres: lda #$8F
            sta ZPtrA + XByte
            sta ZPtrB + XByte
            ldx #$03
dmeval:     lda TargDX, x
            cmp #$80                        ; none close (or on the board at all)
            beq dmnope
            bne dmnotvert
            lda #BrightColor                ; bright arrow on both right and left
            sta MRColL, x
            sta MRColR, x
            lda #(ThinHOffset + LeftArrow)  ; thin on both left and right
            sta MROffsetL, x
            lda #(ThinHOffset + RightArrow) ; thin on both left and right
            sta MROffsetR, x
            jmp dmvcheck
dmnope:     lda #DimColor                   ; dark arrow in all directions
            sta MRColL, x
            sta MRColR, x
            sta MRColU, x
            sta MRColD, x
            lda #(ThinHOffset + LeftArrow)  ; thin on both left and right
            sta MROffsetL, x
            lda #(ThinHOffset + RightArrow) ; thin on both left and right
            sta MROffsetR, x
            lda #(ThinVOffset + UpArrow)    ; thin on both top and bottom
            sta MROffsetU, x
            lda #(ThinVOffset + DownArrow)  ; thin on both top and bottom
            sta MROffsetD, x
            jmp dmchkdone
            ; there is some kind of logic oddness here -- I was sure that
            ; I wanted the next thing to be bmi dmleft, except it was
            ; consistently lighting up the wrong horizontal arrow.
            ; I don't think I am drawing them backwards, so for now I will
            ; just succumb to laziness and apply this fix without quite
            ; understanding why it works this way.
dmnotvert:  bpl dmleft
            eor #$FF                        ; I will forgo adding 1, close enough
            and #$70
            bne dmhdimr                     ; far, use thin arrow
            lda #(FatHOffset + RightArrow)  ; fat arrow
            jmp dmhcontr
dmhdimr:    lda #(ThinHOffset + RightArrow) ; thin arrow
dmhcontr:   sta MROffsetR, x
            lda #(ThinHOffset + LeftArrow)  ; thin on left
            sta MROffsetL, x
            lda #DimColor                   ; dark on left
            sta MRColL, x
            lda #BrightColor                ; bright on right
            sta MRColR, x
            jmp dmvcheck
dmleft:     and #$70
            bne dmhdiml                     ; far, use thin arrow
            lda #(FatHOffset + LeftArrow)   ; fat arrow
            jmp dmhcontl
dmhdiml:    lda #(ThinHOffset + LeftArrow)  ; thin arrow
dmhcontl:   sta MROffsetL, x
            lda #(ThinHOffset + RightArrow) ; thin on right
            sta MROffsetR, x
            lda #DimColor                   ; dark on right
            sta MRColR, x
            lda #BrightColor                ; bright on left
            sta MRColL, x
dmvcheck:   lda TargDY, x
            bne dmnothoriz
            lda #BrightColor                ; bright arrow on both top and bottom
            sta MRColU, x
            sta MRColD, x
            lda #(ThinVOffset + UpArrow)    ; thin on both top and bottom
            sta MROffsetU, x
            lda #(ThinVOffset + DownArrow)  ; thin on both top and bottom
            sta MROffsetD, x
            jmp dmchkdone
dmnothoriz: bmi dmup
            and #$60
            bne dmvdimd                     ; far, use thin arrow
            lda #(FatVOffset + DownArrow)   ; fat arrow
            jmp dmvcontd
dmvdimd:    lda #(ThinVOffset + DownArrow)  ; thin arrow
dmvcontd:   sta MROffsetD, x
            lda #(ThinVOffset + UpArrow)    ; thin up
            sta MROffsetU, x
            lda #DimColor                   ; dark up
            sta MRColU, x
            lda #BrightColor                ; bright down
            sta MRColD, x
            jmp dmchkdone
dmup:       eor #$FF                        ; close enough
            and #$60
            bne dmvdimu                     ; far, use thin arrow
            lda #(FatVOffset + UpArrow)     ; fat arrow
            jmp dmvcontu
dmvdimu:    lda #(ThinVOffset + UpArrow)    ; thin arrow
dmvcontu:   sta MROffsetU, x
            lda #(ThinVOffset + DownArrow)  ; thin down
            sta MROffsetD, x
            lda #DimColor                   ; dark down
            sta MRColD, x
            lda #BrightColor                ; bright up
            sta MRColU, x
dmchkdone:  dex
            bmi dmdraw
            jmp dmeval
            ; at this point should have all the offsets and
            ; colors set, so we can start to draw
dmdraw:     lda R_BANK
            sta DMBank
            lda #$00            ; switch to bank 0, for graphics
            sta R_BANK
            lda #$B0            ; first line
            sta MedLine
            ; stage all the pixel data in the stack for top group
            lda #$07            ; draw 8 lines
            sta MRLines
mrlinea:    ldx #$03
mrpushpixa: ldy MROffsetL, x    ; left arrow
            inc MROffsetL, x    ; consume
            lda MRCompass, y    ; get the shape data
            pha                 ; push for sending out later
            ldy MROffsetU, x    ; up arrow
            inc MROffsetU, x    ; consume
            lda MRCompass, y    ; get the shape data
            pha                 ; push for sending out later
            iny
            inc MROffsetU, x    ; consume
            lda MRCompass, y    ; get the shape data
            pha                 ; push for sending out later
            ldy MROffsetR, x    ; right arrow
            inc MROffsetR, x    ; consume
            lda MRCompass, y    ; get the shape data
            pha                 ; push for sending out later
            dex
            bpl mrpushpixa
            jsr mremit          ; emit pixel data for this line
            inc MedLine
            dec MRLines
            bpl mrlinea
            ; stage all the pixel data in the stack for bottom group
            lda #$07            ; draw 8 lines
            sta MRLines
mrlineb:    ldx #$03
mrpushpixb: ldy MROffsetL, x    ; left arrow
            inc MROffsetL, x    ; consume
            lda MRCompass, y    ; get the shape data
            pha                 ; push for sending out later
            ldy MROffsetD, x    ; down arrow
            inc MROffsetD, x    ; consume
            lda MRCompass, y    ; get the shape data
            pha                 ; push for sending out later
            iny
            inc MROffsetD, x    ; consume
            lda MRCompass, y    ; get the shape data
            pha                 ; push for sending out later
            ldy MROffsetR, x    ; right arrow
            inc MROffsetR, x    ; consume
            lda MRCompass, y    ; get the shape data
            pha                 ; push for sending out later
            dex
            bpl mrpushpixb
            jsr mremit          ; emit pixel data for this line
            inc MedLine
            dec MRLines
            bpl mrlineb
            ; emit color data (no need to stage)
            lda #$B0            ; first line
            sta MedLine
            ; top group
            lda #$07            ; draw 8 lines
            sta MRLines
mrclinea:   ldy MedLine
            lda YHiresHA, y
            clc
            adc #$20            ; compute color page
            sta R_ZP            ; point ZP at the line we will draw
            lda YHiresL, y
            clc
            adc #$1E            ; rightmost one starts at byte +$20
            tax
            ldy #$03
mrpushcola: lda MRColL, y       ; left arrow
            bne :+
            lda DiskColors, y
            and #$F0
            ora #BackColor
:           sta Zero, x
            inx
            lda MRColU, y       ; up arrow
            bne :+
            lda DiskColors, y
            and #$F0
            ora #BackColor
:           sta Zero, x
            inx
            sta Zero, x
            inx
            lda MRColR, y       ; right arrow
            bne :+
            lda DiskColors, y
            and #$F0
            ora #BackColor
:           sta Zero, x
            txa
            sec
            sbc #$0B
            tax
            dey
            bpl mrpushcola
            inc MedLine
            dec MRLines
            bpl mrclinea
            ; bottom group
            lda #$07            ; draw 8 lines
            sta MRLines
mrclineb:   ldy MedLine
            lda YHiresHA, y
            clc
            adc #$20            ; compute color page
            sta R_ZP            ; point ZP at the line we will draw
            lda YHiresL, y
            clc
            adc #$1E            ; rightmost one starts at byte +$20
            tax
            ldy #$03
mrpushcolb: lda MRColL, y       ; left arrow
            bne :+
            lda DiskColors, y
            and #$F0
            ora #BackColor
:           sta Zero, x
            inx
            lda MRColD, y       ; down arrow
            bne :+
            lda DiskColors, y
            and #$F0
            ora #BackColor
:           sta Zero, x
            inx
            sta Zero, x
            inx
            lda MRColR, y       ; right arrow
            bne :+
            lda DiskColors, y
            and #$F0
            ora #BackColor
:           sta Zero, x
            txa
            sec
            sbc #$0B
            tax
            dey
            bpl mrpushcolb
            inc MedLine
            dec MRLines
            bpl mrclineb
            ; all done
DMBank = *+1
            lda #INLINEVAR      ; restore bank 
            sta R_BANK
            lda #$1A            ; restore ZP
            sta R_ZP
            rts

; move pixel data from stack where it was collected to screen
; +23-20, +1B-18 +13-20 +0B-08
mremit:     pla                 ; since we are consuming the stack
            sta MRRetB          ; stash the return value for replacing
            pla
            sta MRRetA
            ldy MedLine
            lda YHiresHA, y
            sta R_ZP            ; point ZP at the line we will draw
            lda YHiresL, y
            clc
            adc #$01            ; stop condition
            sta MREmitted
            clc
            adc #$20            ; rightmost one starts at byte +$21
            tax                 ; (counts back to $1E)
mrdrawpix:  pla
            sta Zero, x
            dex
            dex
            pla
            sta Zero, x
            inx
            pla
            sta Zero, x
            dex
            dex
            pla
            sta Zero, x
            txa
            sec
            sbc #$05            ; back up to end of next block
            tax
MREmitted = *+1
            cmp #INLINEVAR      ; would land on 3 after last one is done
            bne mrdrawpix
MRRetA = *+1
            lda #INLINEVAR      ; replace return value on the stack
            pha
MRRetB = *+1
            lda #INLINEVAR
            pha
            rts
            
MRCompass:                      ; pixels are reversed (LSB left MSB right)
            .byte   %00000000
            .byte   %00000000
            .byte   %00000010
            .byte   %00000110
            .byte   %00001110
            .byte   %00011110
            .byte   %00111100
            .byte   %01111100
            
            .byte   %00111100
            .byte   %00011110
            .byte   %00001110
            .byte   %00000110
            .byte   %00000010
            .byte   %00000000
            .byte   %00000000
            .byte   %00000000

            .byte   %00000000
            .byte   %00000000
            .byte   %00000010
            .byte   %00000110
            .byte   %00001100
            .byte   %00011000
            .byte   %00110000
            .byte   %01100000
            
            .byte   %00110000
            .byte   %00011000
            .byte   %00001100
            .byte   %00000110
            .byte   %00000010
            .byte   %00000000
            .byte   %00000000
            .byte   %00000000

            .byte   %00000000
            .byte   %00000000
            .byte   %01000000
            .byte   %01100000
            .byte   %01110000
            .byte   %01111000
            .byte   %00111100
            .byte   %00111110
            
            .byte   %00111100
            .byte   %01111000
            .byte   %01110000
            .byte   %01100000
            .byte   %01000000
            .byte   %00000000
            .byte   %00000000
            .byte   %00000000

            .byte   %00000000
            .byte   %00000000
            .byte   %01000000
            .byte   %01100000
            .byte   %00110000
            .byte   %00011000
            .byte   %00001100
            .byte   %00000110
            
            .byte   %00001100
            .byte   %00011000
            .byte   %00110000
            .byte   %01100000
            .byte   %01000000
            .byte   %00000000
            .byte   %00000000
            .byte   %00000000

            ; code accommodates for the byte reversal here
            ; because it's easier to be able to see in the source

            .byte   %00000001, %00000000
            .byte   %00000011, %01000000
            .byte   %00001111, %01110000
            .byte   %00111111, %01111100
            .byte   %01111000, %00011110
            .byte   %00000000, %00000000
            .byte   %00000000, %00000000
            .byte   %00000000, %00000000
             
            .byte   %00000001, %00000000
            .byte   %00000011, %01000000
            .byte   %00001110, %01110000
            .byte   %00111000, %00011100
            .byte   %01000000, %00000010
            .byte   %00000000, %00000000
            .byte   %00000000, %00000000
            .byte   %00000000, %00000000
             
            .byte   %00000000, %00000000
            .byte   %00000000, %00000000
            .byte   %00000000, %00000000
            .byte   %01111000, %00011110
            .byte   %00111111, %01111100
            .byte   %00001111, %01110000
            .byte   %00000011, %01000000
            .byte   %00000001, %00000000

            .byte   %00000000, %00000000
            .byte   %00000000, %00000000
            .byte   %00000000, %00000000
            .byte   %01000000, %00000010
            .byte   %00111000, %00011100
            .byte   %00001110, %01110000
            .byte   %00000011, %01000000
            .byte   %00000001, %00000000

;            .byte       %00000000, %00000001, %00000000, %00000000
;            .byte       %00000000, %00000011, %01000000, %00000000
;            .byte       %00000010, %00001111, %01110000, %01000000
;            .byte       %00000110, %00111111, %01111100, %01100000
;            .byte       %00001110, %01111000, %00011110, %01110000
;            .byte       %00011110, %00000000, %00000000, %01111000
;            .byte       %00111100, %00000000, %00000000, %00111100
;            .byte       %01111100, %00000000, %00000000, %00111110

;            .byte       %00111100, %00000000, %00000000, %00111100
;            .byte       %00011110, %00000000, %00000000, %01111000
;            .byte       %00001110, %00000000, %00000000, %01110000
;            .byte       %00000110, %01111000, %00011110, %01100000
;            .byte       %00000010, %00111111, %01111100, %01000000
;            .byte       %00000000, %00001111, %01110000, %00000000
;            .byte       %00000000, %00000011, %01000000, %00000000
;            .byte       %00000000, %00000001, %00000000, %00000000

;            .byte       %00000000, %00000001, %00000000, %00000000
;            .byte       %00000000, %00000011, %01000000, %00000000
;            .byte       %00000010, %00001110, %01110000, %01000000
;            .byte       %00000110, %00111000, %00011100, %01100000
;            .byte       %00001100, %01000000, %00000010, %00110000
;            .byte       %00011000, %00000000, %00000000, %00011000
;            .byte       %00110000, %00000000, %00000000, %00001100
;            .byte       %01100000, %00000000, %00000000, %00000110
            
;            .byte       %00110000, %00000000, %00000000, %00001100
;            .byte       %00011000, %00000000, %00000000, %00011000
;            .byte       %00001100, %00000000, %00000000, %00110000
;            .byte       %00000110, %01000000, %00000010, %01100000
;            .byte       %00000010, %00111000, %00011100, %01000000
;            .byte       %00000000, %00001110, %01110000, %00000000
;            .byte       %00000000, %00000011, %01000000, %00000000
;            .byte       %00000000, %00000001, %00000000, %00000000
