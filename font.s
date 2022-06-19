; Upload special font characters representing the numbers 0 to 9 rolling in one-line
; increments.
; Intentionally cribbed from the 2513 character set so that it will look surprising
; to people who do not think Apples do that.
; Takes advantage of the structure of the font data, bumping the offset one line
; at a time provides the "roll" for free.
; This preserves some of the alpha characters for regular use.

; You can use:
; @ABCDEFGHIJKLMNO
; PQRSTUVWXYZ[\]^_
;  !"#$%&'
;         89:;<=>?
;
; You CANNOT use:
;         ()*+,-./
; 01234567
;
; To use unshifted numbers, you basically need to look them up in
; FONTCHAR.  Hi bit set returns normal, clear returns inverse.
; The unrolled number 2 is: LDY #$02 ; LDA FONTCHAR, y 

CWRTON      = $C0DB
CWRTOFF     = $C0DA
CTEMP       = $A4
DataHoleP   = $A0
AsciiHoleP  = $A2

FontIndex:  .byte $0    ; current char in font being processed
LastChar:   .byte $0    ; ASCII value the last char in group is recorded as
CurrChar:   .byte $0    ; current ASCII value
FDataPtr:   .byte $0    ; offset into font data of the last line of character
LastHole:   .byte $0    ; bottom screen hole index of current group
CurrHole:   .byte $0    ; current screen hole index of current group
FontStep:   .byte $0    ; ASCII value or offset of the current char

HolesL:     .byte   $78, $F8, $78, $F8, $78, $F8, $78, $F8
HolesH:     .byte   $04, $04, $05, $05, $06, $06, $07, $07
HiholesH:   .byte   $08, $08, $09, $09, $0A, $0A, $0B, $0B

; each screen hole holds two rasters of four different characters.
; so first low hole needs: c0r0 c1r0 c2r0 c3r0 c0r1 c1r1 c1r2 c1r3
; and first high hole needs: c0 c1 c2 c3 c0 c1 c2 c3
; then fifth holes move on to c4-c7.

UploadFont: 
            ; even though we do not intentionally use extended addressing,
            ; the code would trigger it.  So we need to set the X-byte to #$8F.
            lda #$8F
            sta DataHoleP+CExtPG
            sta AsciiHoleP+CExtPG
            lda #$00
            sta FontIndex   ; index into font data
WriteRoll:  tay             ; use Y to look up ASCII base code
            asl             ; last font data is index * 8...
            asl
            asl
            clc             ; ...plus 7 (end of character data)... 
            adc #$0F        ; ...plus 8 (each adds only a line bc we are rolling)
            sta FDataPtr    ; last line of last rolling variant for this character
            lda FONTCHAR, y ; get the ASCII base code for these variants
            clc
            adc #$07        ; find the last ASCII code for this variant
            sta CurrChar
            lda #$07
            sta LastHole    ; bottom of current screen hole group
            ldy #$07
movechar:   ldx LastHole
            stx CurrHole
fillhole:   lda HolesL, x   ; set up the data destination pointer
            sta DataHoleP
            sta AsciiHoleP  ; set up the ascii code destination pointer (page 2)
            lda HolesH, x
            sta DataHoleP + 1
            lda HiholesH, x
            sta AsciiHoleP + 1
            ldx FDataPtr    ; move the next two bytes of data into the screen hole
            lda FONTDATA, x
            sta (DataHoleP), y
            lda CurrChar    ; store the ASCII code for both bytes
            sta (AsciiHoleP), y
            dex
            dey             ; move Y down to the first half of the screen hole
            dey
            dey
            dey
            sta (AsciiHoleP), y
            lda FONTDATA, x
            sta (DataHoleP), y
            dex
            stx FDataPtr    ; update the font data pointer
            iny             ; return Y to the second half of the screen hole
            iny
            iny
            iny
            dec CurrHole    ; move up to the next screen hole
            bmi :+
            ldx CurrHole
            cpx #$03
            bne fillhole    ; continue working up the screen holes
            ; finished a character
:           dec CurrChar
            ; because we're rolling the characters, move the data pointer up
            ; to penultimate line of character we just finished
            lda FDataPtr
            clc
            adc #$07
            sta FDataPtr
            dey             ; move Y back one slot in the screen holes
            bmi :+
            cpy #$03
            bne movechar    ; go back to the bottom hole and do next character
            ; finished a hole group, move up to the next one
:           lda LastHole
            sec
            sbc #$04
            bmi sendchars   ; finished the last one, we are done
            sta LastHole
            ldy #$07
            jmp movechar

            ; after each full roll, we have done 8 characters
            ; so it is time to move it to character RAM
            ; entered with either $60 (first time) or $20 (second time)
            ; from p70 Fig 14
            ; CB2 control:
            ; $20 = independent interrupt, input neg edge, CB1 neg act edge
            ; $60 = independent interrupt, input pos edge, CB1 neg act edge
            ; cribbed from the monitor ROM
            
sendchars:  lda #$60
            bit CWRTON
            jsr WaitVBL
            lda #$20
            jsr WaitVBL
            bit CWRTOFF
            
            inc FontIndex
            lda FontIndex
            cmp #$0A
            beq :+
            jmp WriteRoll
:           rts

WaitVBL:    sta CTEMP       ; save bits to be stored
            lda RegPerCtrlE ; control port for CB2
            and #$3F        ; reset hi bits to 0
            ora CTEMP 
            sta RegPerCtrlE
            lda #$08        ; test vertical retrace
            sta RegIntFlagE
:           bit RegIntFlagE ; wait for retrace
            beq :-
            rts
            
; below are character numbers for the beginning of each group of 8
; these are set up this way in order to try to leave the alpha characters
; available for use.
;
; Might be worth swapping in a couple later.  Could maybe map:
;   ;<>89 to
;   /(),.
; if it turns out to be useful. <YOU KNOW8 FOR KIDS;ADULTS9>

FONTCHAR:
            .byte   $00 ; 0
            .byte   $08
            .byte   $10
            .byte   $18
            .byte   $28
            .byte   $30
            .byte   $60
            .byte   $68
            .byte   $70
            .byte   $78 ; 9

; font data - note it is horizontally flipped, hi bit not used

FONTDATA:
            .byte   %00000000   ; 0
            .byte   %00011100
            .byte   %00100010
            .byte   %00110010
            .byte   %00101010
            .byte   %00100110
            .byte   %00100010
            .byte   %00011100
            
            .byte   %00000000   ; 1
            .byte   %00001000
            .byte   %00001100
            .byte   %00001000
            .byte   %00001000
            .byte   %00001000
            .byte   %00001000
            .byte   %00011100

            .byte   %00000000   ; 2
            .byte   %00011100
            .byte   %00100010
            .byte   %00100000
            .byte   %00011000
            .byte   %00000100
            .byte   %00000010
            .byte   %00111110

            .byte   %00000000   ; 3
            .byte   %00111110
            .byte   %00100000
            .byte   %00010000
            .byte   %00011000
            .byte   %00100000
            .byte   %00100010
            .byte   %00011100

            .byte   %00000000   ; 4
            .byte   %00010000
            .byte   %00011000
            .byte   %00010100
            .byte   %00010010
            .byte   %00111110
            .byte   %00010000
            .byte   %00010000

            .byte   %00000000   ; 5
            .byte   %00111110
            .byte   %00000010
            .byte   %00011110
            .byte   %00100000
            .byte   %00100000
            .byte   %00100010
            .byte   %00011100
            
            .byte   %00000000   ; 6
            .byte   %00111000
            .byte   %00000100
            .byte   %00000010
            .byte   %00011110
            .byte   %00100010
            .byte   %00100010
            .byte   %00011100
            
            .byte   %00000000   ; 7
            .byte   %00111110
            .byte   %00100000
            .byte   %00010000
            .byte   %00001000
            .byte   %00000100
            .byte   %00000100
            .byte   %00000100
            
            .byte   %00000000   ; 8
            .byte   %00011100
            .byte   %00100010
            .byte   %00100010
            .byte   %00011100
            .byte   %00100010
            .byte   %00100010
            .byte   %00011100

            .byte   %00000000   ; 9
            .byte   %00011100
            .byte   %00100010
            .byte   %00100010
            .byte   %00111100
            .byte   %00100000
            .byte   %00010000
            .byte   %00001110

            .byte   %00000000   ; 0
            .byte   %00011100
            .byte   %00100010
            .byte   %00110010
            .byte   %00101010
            .byte   %00100110
            .byte   %00100010
            .byte   %00011100

