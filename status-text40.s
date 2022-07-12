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
            .byte "00 ", C_DISK, " 00 2 "
            .byte "00 ", C_DISK, " 00 3 "
            .byte "00 ", C_DISK, " 00 4 "

ProgColA:   .byte $0F, $0F, $0F, $0E, $0F, $0E, $0E, $0F, $E1, $0F
            .byte $0F, $0F, $0F, $0D, $0F, $0E, $0E, $0F, $E1, $0F
            .byte $0F, $0F, $0F, $0C, $0F, $0E, $0E, $0F, $E1, $0F
            .byte $0F, $0F, $0F, $0B, $0F, $0E, $0E, $0F, $E1, $0F

; paint the initial background

TEXT1CHAR   = $400              ; text line 1
TEXT1COL    = $800              ; colors line 1
TEXT2CHAR   = $480              ; text line 2
TEXT2COL    = $880              ; colors line 2

initstatus: ldy #$27
:           lda StatTextA, y
            sta TEXT1CHAR, y
            lda StatColA, y
            sta TEXT1COL, y
            lda ProgTextA, y
            sta TEXT2CHAR, y
            lda ProgColA, y
            sta TEXT2COL, y
            dey
            bpl :-
            ; fall through to drawlevel
            
; update level on screen - kept separate because it rarely changes

drawlevel:  lda #$07            ; update level - $407 is screen location of level
            sta ZNumPtr
            lda #$04
            sta ZNumPtr + 1
            lda #$8F
            sta ZNumPtr + XByte
            lda GameLevel
            jmp drawnumber

; draw the score and progress

CharGot:    .byte $80, $8A, $94, $9E
CharLeft:   .byte $85, $8F, $99, $A3

drawstatus: lda #$16            ; update score - $416 is screen location of score (end)
            sta ZNumPtr
            lda #$04
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
            ldx #$03            ; update disk types gotten and left
:           lda CharGot, x
            sta ZNumPtr
            lda DisksGot, x
            jsr drawnumber
            lda CharLeft, x
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
