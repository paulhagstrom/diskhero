; DISKHERO
; Upload special font characters for the diskhero game
;
; called once at the beginning to move the data, not again
; so it can be in lower memory that gets switched out

BlockCount: .byte $0    ; countdown of 8-character blocks to send
CurrChar:   .byte $0    ; index to table with current ASCII value
FDataPtr:   .byte $0    ; offset into font data of the last line of character
CurrHole:   .byte $0    ; current screen hole index of current group
HoleSlice:  .byte $0    ; current bit (of 4) in the screen hole

HolesL:     .byte   $78, $F8, $78, $F8, $78, $F8, $78, $F8
HolesH:     .byte   $04, $04, $05, $05, $06, $06, $07, $07
HiholesH:   .byte   $08, $08, $09, $09, $0A, $0A, $0B, $0B

; each screen hole holds two rasters of four different characters.
; so first low hole needs: c0r0 c1r0 c2r0 c3r0 c0r1 c1r1 c2r1 c3r1
; and first high hole needs: c0 c1 c2 c3 c0 c1 c2 c3
; then fifth holes move on to c4-c7.
; 8 characters are transmitted at a time, 8 bytes/lines per character, so $40 bytes per block.
; assume we are in $1A00 ZP (allowing for extended addressing, though we 8F-disable it)

herofont:   ldx #$27            ; pull the FontDots and FontCol info into ZP for fast/durable access
:           lda FontDots, x
            sta ZFontDots, x
            lda FontCol, x
            sta ZFontCol, x
            dex
            bpl :-
            lda #$8F                ; stay in s-bank
            sta ZDataHole + XByte
            sta ZAsciiHole + XByte
            sta ZFontPtr + XByte
            lda #$04                ; sending 5 blocks of characters
            sta BlockCount
            lda #<FontData
            sta ZFontPtr
            lda #>FontData
            sta ZFontPtr + 1        ; point ZFontPtr to the font data, first block
            lda #$07                ; CurrChar indexes the current character code
            sta CurrChar            ; counts backwards within a block, so this is last in first block
writeblock: lda #$3F
            sta FDataPtr            ; FDataPtr indexes the last byte of data in this block
            lda #$03
            sta HoleSlice           ; the bit in the screen hole we're on (of 4)
movechar:   ldx #$07                ; current screen hole
fillhole:   lda HolesL, x           ; set up destination pointers into current lo/hi screen holes
            sta ZDataHole           ; bitmap destination (page 1)
            sta ZAsciiHole          ; ascii code destination (page 2)
            lda HolesH, x
            sta ZDataHole + 1
            lda HiholesH, x
            sta ZAsciiHole + 1
            ldy CurrChar            ; load the character code
            lda FontAscii, y
            pha
            ldy FDataPtr            ; load the next two font line bitmaps
            lda (ZFontPtr), y
            pha
            dey
            lda (ZFontPtr), y
            dey
            sty FDataPtr
            ldy HoleSlice           ; store the font data
            sta (ZDataHole), y
            pla
            iny
            iny
            iny
            iny
            sta (ZDataHole), y      ; store the character code
            pla
            sta (ZAsciiHole), y
            ldy HoleSlice
            sta (ZAsciiHole), y
            dex                     ; move up to the next screen hole
            bmi :+                  ; done with both characters at this slice
            cpx #$03                ; passed the middle so on to second character
            bne fillhole            ; continue walking up the screen holes
            dec CurrChar
            bpl fillhole            ; branch always
:           dec CurrChar
            dec HoleSlice           ; move on to next bit in the screen hole
            bpl movechar
            jsr sendchars           ; finished a block, send it to character RAM
            dec BlockCount
            bmi fontdone
            lda ZFontPtr            ; move to the next block
            clc
            adc #$40
            sta ZFontPtr
            bcc :+
            inc ZFontPtr + 1
:           lda CurrChar            ; move CurrChar up to point at last character in next block
            clc
            adc #$10
            sta CurrChar
            jmp writeblock
            
sendchars:  lda #$60                ; $60 = ind int, inp pos edge, CB1 neg act edge
            bit F_XFERON            ; cribbed from the monitor ROM
            jsr waitVBL
            lda #$20                ; $20 = ind int, inp neg edge, CB1 neg act edge
            jsr waitVBL
            bit F_XFEROFF
fontdone:   rts

waitVBL:    sta ZCtlTemp    ; save bits to be stored
            lda RE_PERCTL   ; control port for CB2
            and #$3F        ; reset hi bits to 0
            ora ZCtlTemp 
            sta RE_PERCTL
            lda #$08        ; test vertical retrace
            sta RE_INTFLAG
:           bit RE_INTFLAG  ; wait for retrace
            beq :-
            rts

; the ASCII values of characters we are defining.
; needs to be in the same order as the FontData below it, of course.

FontAscii:
            .byte   $00         ; space
            .byte   $01         ; wall right end
            .byte   $02         ; wall right down
            .byte   $03         ; wall right up
            .byte   $04         ; wall right up down
            .byte   $05         ; wall horiz
            .byte   $06         ; wall left end
            .byte   $07         ; wall left down
            .byte   $08         ; wall left up
            .byte   $09         ; wall left up down
            .byte   $0A         ; wall vert
            .byte   $0B         ; wall up end
            .byte   $0C         ; wall down end
            .byte   $0D         ; wall left right up
            .byte   $0E         ; wall left right down
            .byte   $0F         ; disk
            .byte   $10         ; hero
            .byte   $11         ; hoarder head 1
            .byte   $12         ; hoarder head 2
            .byte   $13         ; hoarder hands up 1
            .byte   $14         ; hoarder hands up 2
            .byte   $15         ; hoarder hands down 1
            .byte   $16         ; hoarder hands down 2
            .byte   $17         ; hoarder hands right 1
            .byte   $18         ; hoarder hands right 2
            .byte   $19         ; hoarder hands left 1
            .byte   $1A         ; hoarder hands left 2
            .byte   $1B         ; drive left
            .byte   $1C         ; drive right
            .byte   $1D         ; drive up
            .byte   $1E         ; drive down
            .byte   $1F         ; flux 1
            .byte   $28         ; flux 2 -- leave $20 for space
            .byte   $21         ; truck left A
            .byte   $22         ; truck left B
            .byte   $23         ; truck right A
            .byte   $24         ; truck right B
            .byte   $25         ; unused
            .byte   $26         ; unused
            .byte   $27         ; unused

; in the hires map, we have two pixels to represent each
; character.  For walls and disks, color is separate, ANDed with bits here.
; For other characters, this holds the two colors (right pixel, left pixel).
; Note that the nibbles appear here in reverse order because screen significance
; increases rightward.

FontDots:
            .byte   %00000000         ; space
            .byte   %11111111         ; wall right end
            .byte   %11111111         ; wall right down
            .byte   %11111111         ; wall right up
            .byte   %11111111         ; wall right up down
            .byte   %11111111         ; wall horiz
            .byte   %11111111         ; wall left end
            .byte   %11111111         ; wall left down
            .byte   %11111111         ; wall left up
            .byte   %11111111         ; wall left up down
            .byte   %11111111         ; wall vert
            .byte   %11111111         ; wall up end
            .byte   %11111111         ; wall down end
            .byte   %11111111         ; wall left right up
            .byte   %11111111         ; wall left right down
            .byte   %11110000         ; disk
            .byte   %11011101         ; hero
            .byte   %11001100         ; hoarder head 1
            .byte   %11001100         ; hoarder head 2
            .byte   %11001101         ; hoarder hands up 1
            .byte   %11011100         ; hoarder hands up 2
            .byte   %11001101         ; hoarder hands down 1
            .byte   %11011100         ; hoarder hands down 2
            .byte   %11001101         ; hoarder hands right 1
            .byte   %11011100         ; hoarder hands right 2
            .byte   %11001101         ; hoarder hands left 1
            .byte   %11011100         ; hoarder hands left 2
            .byte   %11011101         ; drive left
            .byte   %11011101         ; drive right
            .byte   %11011101         ; drive up
            .byte   %11011101         ; drive down
            .byte   %10101010         ; flux 1
            .byte   %10101010         ; flux 2
            .byte   %10001000         ; truck left A
            .byte   %10001000         ; truck left B
            .byte   %10001000         ; truck right A
            .byte   %10001000         ; truck right B
            .byte   %00000000         ; unused
            .byte   %00000000         ; unused
            .byte   %00000000         ; unused

; on the text playfield, some characters have intrinsic colors
; this table records them.  Characters 0-F use indexed colors, so
; this data is not used for those. This is only for foreground color.
; since I don't remember which nibble is foreground, both nibbles are
; set. :D

FontCol:
            .byte   %11111111         ; space
            .byte   %11111111         ; wall right end
            .byte   %11111111         ; wall right down
            .byte   %11111111         ; wall right up
            .byte   %11111111         ; wall right up down
            .byte   %11111111         ; wall horiz
            .byte   %11111111         ; wall left end
            .byte   %11111111         ; wall left down
            .byte   %11111111         ; wall left up
            .byte   %11111111         ; wall left up down
            .byte   %11111111         ; wall vert
            .byte   %11111111         ; wall up end
            .byte   %11111111         ; wall down end
            .byte   %11111111         ; wall left right up
            .byte   %11111111         ; wall left right down
            .byte   %11111111         ; disk
            .byte   %11011101         ; hero
            .byte   %11001100         ; hoarder head 1
            .byte   %11001100         ; hoarder head 2
            .byte   %11001100         ; hoarder hands up 1
            .byte   %11001100         ; hoarder hands up 2
            .byte   %11001100         ; hoarder hands down 1
            .byte   %11001100         ; hoarder hands down 2
            .byte   %11001100         ; hoarder hands right 1
            .byte   %11001100         ; hoarder hands right 2
            .byte   %11001100         ; hoarder hands left 1
            .byte   %11001100         ; hoarder hands left 2
            .byte   %11011101         ; drive left
            .byte   %11011101         ; drive right
            .byte   %11011101         ; drive up
            .byte   %11011101         ; drive down
            .byte   %10101010         ; flux 1
            .byte   %10101010         ; flux 2
            .byte   %10001000         ; truck left A
            .byte   %10001000         ; truck left B
            .byte   %10001000         ; truck right A
            .byte   %10001000         ; truck right B
            .byte   %00000000         ; unused
            .byte   %00000000         ; unused
            .byte   %00000000         ; unused

; name the characters so they can be referred to

C_SPACE     = $00         ; space
C_WALL_R    = $01         ; wall right end
C_WALL_RD   = $02         ; wall right down
C_WALL_RU   = $03         ; wall right up
C_WALL_RUD  = $04         ; wall right up down
C_WALL_H    = $05         ; wall horiz
C_WALL_L    = $06         ; wall left end
C_WALL_LD   = $07         ; wall left down
C_WALL_LU   = $08         ; wall left up
C_WALL_LUD  = $09         ; wall left up down
C_WALL_V    = $0A         ; wall vert
C_WALL_U    = $0B         ; wall up end
C_WALL_D    = $0C         ; wall down end
C_WALL_LRU  = $0D         ; wall left right up
C_WALL_LRD  = $0E         ; wall left right down
C_DISK      = $0F         ; disk
C_HERO      = $10         ; hero
C_HHEADA    = $11         ; hoarder head 1
C_HHEADB    = $12         ; hoarder head 2
C_HHANDUA   = $13         ; hoarder hands up 1
C_HHANDUB   = $14         ; hoarder hands up 2
C_HHANDDA   = $15         ; hoarder hands down 1
C_HHANDDB   = $16         ; hoarder hands down 2
C_HHANDRA   = $17         ; hoarder hands right 1
C_HHANDRB   = $18         ; hoarder hands right 2
C_HHANDLA   = $19         ; hoarder hands left 1
C_HHANDLB   = $1A         ; hoarder hands left 2
C_DRIVEL    = $1B         ; drive left
C_DRIVER    = $1C         ; drive right
C_DRIVEU    = $1D         ; drive up
C_DRIVED    = $1E         ; drive down
C_FLUXA     = $1F         ; flux 1
C_FLUXB     = $28         ; flux 2 -- leave $20 for space
C_TRUCKLA   = $21         ; truck left A
C_TRUCKLB   = $22         ; truck left B
C_TRUCKRA   = $23         ; truck right A
C_TRUCKRB   = $24         ; truck right B
C_UNUA      = $25         ; unused
C_UNUB      = $26         ; unused
C_UNUC      = $27         ; unused

; font data - note it is horizontally flipped, hi bit not used

FontData:

; Walls
            .byte   %00000000   ; space
            .byte   %00000000
            .byte   %00000000
            .byte   %00000000
            .byte   %00000000
            .byte   %00000000
            .byte   %00000000
            .byte   %00000000

            .byte   %00000000   ; wall right end
            .byte   %00000000
            .byte   %01111100
            .byte   %01111100
            .byte   %01111100
            .byte   %01111100
            .byte   %00000000
            .byte   %00000000

            .byte   %00000000   ; wall right down
            .byte   %00000000
            .byte   %01111100
            .byte   %01111100
            .byte   %01111100
            .byte   %01111100
            .byte   %00011100
            .byte   %00011100
            
            .byte   %00011100   ; wall right up
            .byte   %00011100
            .byte   %01111100
            .byte   %01111100
            .byte   %01111100
            .byte   %01111100
            .byte   %00000000
            .byte   %00000000
            
            .byte   %00011100   ; wall right up down
            .byte   %00011100
            .byte   %01111100
            .byte   %01111100
            .byte   %01111100
            .byte   %01111100
            .byte   %00011100
            .byte   %00011100

            .byte   %00000000   ; wall horiz
            .byte   %00000000
            .byte   %01111111
            .byte   %01111111
            .byte   %01111111
            .byte   %01111111
            .byte   %00000000
            .byte   %00000000

            .byte   %00000000   ; wall left end
            .byte   %00000000
            .byte   %00011111
            .byte   %00011111
            .byte   %00011111
            .byte   %00011111
            .byte   %00000000
            .byte   %00000000
            
            .byte   %00000000   ; wall left down
            .byte   %00000000
            .byte   %00011111
            .byte   %00011111
            .byte   %00011111
            .byte   %00011111
            .byte   %00011100
            .byte   %00011100

            .byte   %00011100   ; wall left up
            .byte   %00011100
            .byte   %00011111
            .byte   %00011111
            .byte   %00011111
            .byte   %00011111
            .byte   %00000000
            .byte   %00000000
                        
            .byte   %00011100   ; wall left up down
            .byte   %00011100
            .byte   %00011111
            .byte   %00011111
            .byte   %00011111
            .byte   %00011111
            .byte   %00011100
            .byte   %00011100
            
            .byte   %00011100   ; wall vert
            .byte   %00011100
            .byte   %00011100
            .byte   %00011100
            .byte   %00011100
            .byte   %00011100
            .byte   %00011100
            .byte   %00011100
            
            .byte   %00011100   ; wall up end
            .byte   %00011100
            .byte   %00011100
            .byte   %00011100
            .byte   %00011100
            .byte   %00011100
            .byte   %00000000
            .byte   %00000000

            .byte   %00000000   ; wall down end
            .byte   %00000000
            .byte   %00011100
            .byte   %00011100
            .byte   %00011100
            .byte   %00011100
            .byte   %00011100
            .byte   %00011100
            
            .byte   %00011100   ; wall left right up
            .byte   %00011100
            .byte   %01111111
            .byte   %01111111
            .byte   %01111111
            .byte   %01111111
            .byte   %00000000
            .byte   %00000000
            
            .byte   %00000000   ; wall left right down
            .byte   %00000000
            .byte   %01111111
            .byte   %01111111
            .byte   %01111111
            .byte   %01111111
            .byte   %00011100
            .byte   %00011100

; characters
            .byte   %01111111   ; disk
            .byte   %01111110
            .byte   %01110111
            .byte   %01100011
            .byte   %01110111
            .byte   %01111111
            .byte   %01110111
            .byte   %01110111  

            .byte   %00011100   ; hero
            .byte   %00100010
            .byte   %00011100
            .byte   %01111111
            .byte   %00001000
            .byte   %00010100
            .byte   %00100010
            .byte   %01100011
            
            .byte   %00011100   ; hoarder head 1
            .byte   %00111110
            .byte   %01101011
            .byte   %01111111
            .byte   %01111111
            .byte   %01100011
            .byte   %00100010
            .byte   %00011100

            .byte   %00011100   ; hoarder head 2
            .byte   %00111110
            .byte   %01101011
            .byte   %01101011
            .byte   %01111111
            .byte   %01100011
            .byte   %00111110
            .byte   %00011100

            .byte   %00000000   ; hoarder hands up 1
            .byte   %00000000
            .byte   %01010000
            .byte   %01110000
            .byte   %00110000
            .byte   %00110101
            .byte   %00110111
            .byte   %00110110

            .byte   %00000000   ; hoarder hands up 2
            .byte   %00000000
            .byte   %00000101
            .byte   %00000111
            .byte   %01010110
            .byte   %01110110
            .byte   %00110110
            .byte   %00110110

            .byte   %00110110   ; hoarder hands down 1
            .byte   %00110111
            .byte   %00110101
            .byte   %00110000
            .byte   %01110000
            .byte   %01010000
            .byte   %00000000
            .byte   %00000000
            
            .byte   %00110110   ; hoarder hands down 2
            .byte   %01110110
            .byte   %01010110
            .byte   %00000110
            .byte   %00000111
            .byte   %00000101
            .byte   %00000000
            .byte   %00000000

            .byte   %00000000   ; hoarder hands right 1
            .byte   %01100000
            .byte   %00111111
            .byte   %01111111
            .byte   %00000000
            .byte   %00011111
            .byte   %00001111
            .byte   %00011000
            
            .byte   %00000000   ; hoarder hands right 2
            .byte   %00011000
            .byte   %00001111
            .byte   %00011111
            .byte   %00000000
            .byte   %01111111
            .byte   %00111111
            .byte   %01100000
            
            .byte   %00000000   ; hoarder hands left 1
            .byte   %00000011
            .byte   %01111110
            .byte   %01111111
            .byte   %00000000
            .byte   %01111100
            .byte   %01111000
            .byte   %00001100
            
            .byte   %00000000   ; hoarder hands left 2
            .byte   %00001100
            .byte   %01111000
            .byte   %01111100
            .byte   %00000000
            .byte   %01111111
            .byte   %01111110
            .byte   %00000011
            
            .byte   %00000000   ; drive left
            .byte   %00000000
            .byte   %01111111
            .byte   %00111111
            .byte   %00111111
            .byte   %00010010
            .byte   %00000000
            .byte   %00000000

            .byte   %00000000   ; drive right
            .byte   %00000000
            .byte   %01111111
            .byte   %01111110
            .byte   %01111110
            .byte   %00100100
            .byte   %00000000
            .byte   %00000000

            .byte   %00111110   ; drive up
            .byte   %00111110
            .byte   %00111110
            .byte   %00111110
            .byte   %00111110
            .byte   %00111110
            .byte   %00111110
            .byte   %00011000

            .byte   %00001100   ; drive down
            .byte   %00111110
            .byte   %00111110
            .byte   %00111110
            .byte   %00111110
            .byte   %00111110
            .byte   %00111110
            .byte   %00111110

            .byte   %00011100   ; flux 1
            .byte   %00111110
            .byte   %01110111
            .byte   %00000011
            .byte   %01110111
            .byte   %00111010
            .byte   %00011100
            .byte   %00000000

            .byte   %00011100   ; flux 2
            .byte   %00111110
            .byte   %01010111
            .byte   %01100000
            .byte   %01110111
            .byte   %00111110
            .byte   %00011100
            .byte   %00000000

            .byte   %00000000   ; truck left A 1
            .byte   %00000011
            .byte   %00111011
            .byte   %01111011
            .byte   %01111111
            .byte   %01011011
            .byte   %00111100
            .byte   %00011000
            
            .byte   %00000000   ; truck left B 1
            .byte   %01111100
            .byte   %01111110
            .byte   %01111111
            .byte   %01111111
            .byte   %01101101
            .byte   %00011110
            .byte   %00001100

            .byte   %00000000   ; truck right A 1
            .byte   %00011111
            .byte   %00111111
            .byte   %01111111
            .byte   %01111111
            .byte   %01011011
            .byte   %00111100
            .byte   %00011000

            .byte   %00000000   ; truck right B 1
            .byte   %01100000
            .byte   %01101110
            .byte   %01101111
            .byte   %01111111
            .byte   %01101101
            .byte   %00011110
            .byte   %00001100
            
            .byte   %00000000   ; unused
            .byte   %01111111
            .byte   %01111111
            .byte   %01111111
            .byte   %01111111
            .byte   %01111111
            .byte   %01111111
            .byte   %01111111
            
            .byte   %00000000   ; unused
            .byte   %01111111
            .byte   %01111111
            .byte   %01111111
            .byte   %01111111
            .byte   %01111111
            .byte   %01111111
            .byte   %01111111
            
            .byte   %00000000   ; unused
            .byte   %01111111
            .byte   %01111111
            .byte   %01111111
            .byte   %01111111
            .byte   %01111111
            .byte   %01111111
            .byte   %01111111
            
FontEnd     =       *
