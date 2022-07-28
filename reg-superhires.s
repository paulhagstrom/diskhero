; DISKHERO
; Apple III super-hires region
;
; occupies  lines 00-0F (10)
; displays an animation above the inventory numbers when they change

SplashG:    .byte   $00, $00, $00, $00      ; frame for "got" columns
SplashL:    .byte   $00, $00, $00, $00      ; frame for "left" columns
SplashXG:   .byte   $00, $0C, $15, $21      ; X-coordinates for "got" columns
SplashXL:   .byte   $05, $11, $1A, $26      ; X-coordinates for "left" columns

; splash frame: set SplashG, x or SplashL, x to $50 to start an animation
; will draw a frame, tick down by $10 until complete
; this treats all animations as "got" animations, but when setting the frame
; to start, it's useful to be able to refer to the "left" ones.

updsplash:  lda #$8F
            sta ZPtrA + XByte
            sta ZPtrB + XByte
            lda #$0F                ; do all columns of each line
            sta PatIndex
            lda #$0F                ; draw splashes on lines 08-0F
            sta PatRaster
PatRaster = *+1
splashline: ldx #INLINEVAR
            lda YHiresL, x
            sta ZPtrA
            sta ZPtrB
            lda YHiresHA, x
            sta ZPtrA + 1
            clc
            adc #$20                ; compute HB
            sta ZPtrB + 1
            ldx #$07
            stx SplashCol
SplashCol = *+1
spfcheck:   ldx #INLINEVAR
            lda SplashG, x
            bmi spfcont
            ldy SplashXG, x
            sec
            sbc #$10
            bpl spfreal             ; this is a real frame
            lda #$00                ; this is frame 0, draw blank
            sta (ZPtrB), y
            iny
            sta (ZPtrA), y
            jmp spfcont
spfreal:    clc
PatIndex= *+1
            adc #INLINEVAR
            tax
            lda HR_Splash, x
            sta (ZPtrB), y
            dex
            iny
            lda HR_Splash, x
            sta (ZPtrA), y
spfcont:    dec SplashCol
            bpl spfcheck
            dec PatRaster
            dec PatIndex
            dec PatIndex
            bpl splashline
            ldx #$07
spfupd:     lda SplashG, x
            bmi spfnext
            sec
            sbc #$10
            sta SplashG, x
spfnext:    dex
            bpl spfupd
            rts

; clear the super hires lines 00-0F

initshgr:   ldx #$0F                ; clear from lines 0F to line 00
shgrclear:  lda YHiresL, x
            sta ZPtrA
            sta ZPtrB
            lda YHiresHA, x
            sta ZPtrA + 1
            clc
            adc #$20                ; compute HB
            sta ZPtrB + 1
            lda #$8F
            sta ZPtrA + XByte
            sta ZPtrB + XByte
            ldy #$27
            lda #$00
shgrbyte:   sta (ZPtrA), y
            sta (ZPtrB), y
            dey
            bpl shgrbyte
            dex
            bpl shgrclear
            rts

HR_Disk:
;            .byte       %00000000, %00000111, %01110000, %00000000
;            .byte       %00000000, %00111000, %00001110, %00000000
;            .byte       %00000000, %01000000, %00000001, %00000000
;            .byte       %00000001, %00000000, %00000000, %01000000
;            .byte       %00000010, %00000000, %00000000, %00100000
;            .byte       %00000010, %00000001, %01000000, %00100000
;            .byte       %00000100, %00000010, %00100000, %00010000
;            .byte       %00000100, %00000100, %00010000, %00010000

;            .byte       %00000100, %00000100, %00010000, %00010000
;            .byte       %00000100, %00000010, %00100000, %00010000
;            .byte       %00000010, %00000001, %01000000, %00100000
;            .byte       %00000010, %00000000, %00000000, %00100000
;            .byte       %00000001, %00000000, %00000000, %01000000
;            .byte       %00000000, %01000000, %00000001, %00000000
;            .byte       %00000000, %00111000, %00001110, %00000000
;            .byte       %00000000, %00000111, %01110000, %00000000

HR_Splash:              ; 16 bytes per frame, 5 frames
            .byte       %00000000, %00000000
            .byte       %00000000, %00000000
            .byte       %00000000, %00000000
            .byte       %01000000, %00000000
            .byte       %00000000, %00000010
            .byte       %00010000, %00000000
            .byte       %00000000, %00010000
            .byte       %00000000, %00000000

            .byte       %00000000, %00000000
            .byte       %00000000, %00000000
            .byte       %00000000, %00000000
            .byte       %00010000, %00010000
            .byte       %00000110, %00001000
            .byte       %00001000, %00100010
            .byte       %00011000, %00000000
            .byte       %00000000, %00000000

            .byte       %00000000, %00000000
            .byte       %00000000, %00000000
            .byte       %00000010, %00000000
            .byte       %00000000, %01000100
            .byte       %00000110, %00011000
            .byte       %00001101, %00110000
            .byte       %00000000, %00110000
            .byte       %00000000, %00000000

            .byte       %00000000, %00000000
            .byte       %00000000, %00000000
            .byte       %00000001, %00000000
            .byte       %00000000, %01000000
            .byte       %00000100, %01101000
            .byte       %00000101, %01010000
            .byte       %00000111, %01110000
            .byte       %00000011, %01100000
            
            .byte       %00000000, %00000000
            .byte       %00000000, %00000000
            .byte       %00000000, %00000000
            .byte       %00000000, %00000000
            .byte       %00000000, %00000000
            .byte       %00000000, %00000000
            .byte       %00000001, %01000000
            .byte       %00000011, %01100000

