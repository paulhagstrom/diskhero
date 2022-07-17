; DISKHERO
; Apple III 40-column text region
; top score and progress display
; occupies scan lines 00-0F, text lines 0-1.

StatTextA:  .byte " Level    "
            .byte " Score:   "
            .byte "          "
            .byte " DISKHERO "

StatColA:   .byte $2D, $2D, $2D, $2D, $2D, $2D, $2E, $2E, $2E, $2E
            .byte $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0D, $0D
            .byte $0D, $0D, $0D, $0D, $0D, $F0, $B0, $E0, $C0, $D0
            .byte $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E

ProgTextA:  .byte "00 ", C_DISK, " 00 1 "
            .byte "2 00 ", C_DISK, " 00 "
            .byte " 00 ", C_DISK, " 00 3"
            .byte " 4 00 ", C_DISK, " 00"

ProgColA:   .byte $0F, $0F, $0F, $0E, $0F, $0E, $0E, $0F, $A5, $0F
            .byte $A5, $0F, $0F, $0F, $0F, $0D, $0F, $0E, $0E, $0F
            .byte $0F, $0F, $0F, $0F, $0C, $0F, $0E, $0E, $0F, $A5
            .byte $0F, $A5, $0F, $0F, $0F, $0F, $0B, $0F, $0E, $0E

; paint the initial background

initstatus: lda #$8F
            sta ZPtrA + XByte
            ldx #$03            ; score on line 4
            lda YLoresL, x
            sta ZPtrA
            lda YLoresHA, x
            sta ZPtrA + 1
            ldy #$27
:           lda StatTextA, y
            sta (ZPtrA), y
            dey
            bpl :-
            lda YLoresHB, x
            sta ZPtrA + 1
            ldy #$27
:           lda StatColA, y
            sta (ZPtrA), y
            dey
            bpl :-
            ldx #$02            ; pregress on line 3
            lda YLoresL, x
            sta ZPtrA
            lda YLoresHA, x
            sta ZPtrA + 1
            ldy #$27
:           lda ProgTextA, y
            sta (ZPtrA), y
            dey
            bpl :-
            lda YLoresHB, x
            sta ZPtrA + 1
            ldy #$27
:           lda ProgColA, y
            sta (ZPtrA), y
            dey
            bpl :-
            ; fall through to drawlevel
            
; update level on screen - kept separate because it rarely changes

drawlevel:  ldx #$03
            lda YLoresL, x
            clc
            adc #$07            ; +$07 is screen location of level
            sta ZNumPtr
            lda YLoresHA, x
            sta ZNumPtr + 1
            lda #$8F
            sta ZNumPtr + XByte
            lda GameLevel
            jmp drawnumber

; draw the score and progress

CharGot:    .byte $00, $0C, $15, $21
CharLeft:   .byte $05, $11, $1A, $26

drawstatus: ldx #$03
            lda YLoresL, x
            sta ZPxScratch
            clc
            adc #$16            ; +$16 is screen location of score (end)
            sta ZNumPtr
            lda YLoresHA, x
            sta ZNumPtr + 1
            lda #$8F
            sta ZNumPtr + XByte
            ldx #$02
:           lda GameScore, x
            jsr drawnumber
            dec ZNumPtr
            dec ZNumPtr
            dex
            bpl :-
            ldx #$02
            lda YLoresHA, x
            sta ZNumPtr + 1
            lda YLoresL, x
            sta ZPxScratch
            ldx #$03            ; update disk types gotten and left
:           lda CharGot, x
            clc
            adc ZPxScratch
            sta ZNumPtr
            lda DisksGot, x
            jsr drawnumber
            lda CharLeft, x
            clc
            adc ZPxScratch
            sta ZNumPtr
            lda DisksLeft, x
            jsr drawnumber
            dex
            bpl :-
            ; stick a couple of debug indicators on screen
            ;lda NudgePos
            ;sta $481
            ;lda HeroY
            ;sta $482
            rts

; put a 2-digit number on screen.
; presumed decimal use of a byte (first nibble 10s, second nibble 1s)
; A holds the number, ZNumPtr holds the screen address of the number.
; Will trigger extended addressing, so set ZNumPtr + XByte to 8F.
; A and Y do not survive.

drawnumber: pha
            lsr
            lsr
            lsr
            lsr
            ora #$30
            ldy #$00
            sta (ZNumPtr), y
            pla
            and #$0F
            ora #$30
            iny
            sta (ZNumPtr), y
            rts
