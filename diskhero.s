;
; diskhero
; Paul Hagstrom, 2022
; written for KansasFest 2022 presentation
; Apple /// arcade game

; Interp bank - 2000 on, game code
; S-bank: 400-C00 text/graphics, 2000-9fff graphics
; bank 2: 2000-5FFF - generated map representation


            .setcpu "6502"
            .segment "CODE"
         
            .include "diskhero.inc"

            .org     $2000 - 14

; sos interp header
            .byte    "SOS NTRP"
            .word    0000
            .word    CodeStart
            .word    (CodeEnd - CodeStart)

CodeStart:  jmp Init
            
; put the interrupt handler here at the beginning so that we're
; unlikely to get any 6502 page-crossing penalties

HandleInt:  pha
            tya
            pha
            txa
            pha
            cld
            lda E_IFR
            and #$10        ; CB1 (VBL)
            beq :+          ; not VBL
            jsr DoVBL
            lda #$10        ; clear CB1 VBL
            sta E_IFR
            bne IntReturn
:           lda E_IFR
            and #$01        ; CA2 (Keyboard)
            beq :+          ; not keyboard
            jsr DoKeyboard 
            lda #$01        ; clear CA2 keyboard
            sta E_IFR
            bne IntReturn
:           lda E_IFR
            and #$20        ; timer 2
            bne DoTimer
IntReturn:  pla
            tax
            pla
            tay
            pla
            rti
; timer2 interrupt handler
DoTimer:    lda #$20        ;clear timer2 flag
            sta E_IFR
            ; do game tasks
            ; IntReturn is probably too far away to branch to, so replicate
            pla
            tax
            pla
            tay
            pla
            rti
; keyboard interrupt handler
DoKeyboard: lda KBD
            sta KBDCLEAR
            bpl KeyReturn   ; no key pressed, return
            ; buffer keys
KeyReturn:  rts
; VBL interrupt handler
DoVBL:      ; assume that we are already in display mode 1 (A3 40 text)
            ; top 16 lines are status.  Lines (0-1) come from:
            ; 400-427 (chars) 800-827 (colors)
            ; 480-4A7 (chars) 880-8A7 (colors)
            ; text lines: 0-1, pixels: 0-15
            ldx #16         ; stay in mode 1 for 16 lines
:           bit E_IORB
            bvc :-          ; wait until bit 6 (BL) is set
            dex
            beq :++
:           bit E_IORB
            bvs :-          ; wait until bit 6 (BL) is clear again
            bvc :--         ; wait for the next line
            ; switch to display mode 6 (A3 bw superhires) (map/progress)
            ; 16 lines come from:
            ; 2000 (even) 4000 (odd)
            ; 2080 (even) 4080 (odd)
            ; etc.
            ; draw: 
:           lda GRAPHICS    ; display mode 6 (A3 bw superhires) (map)
            lda HIRES
            lda MIX
            ldx #16         ; stay in mode 6 for 16 lines
:           bit E_IORB
            bvc :-          ; wait until bit 6 (BL) is set
            dex
            beq :++
:           bit E_IORB
            bvs :-          ; wait until bit 6 (BL) is clear again
            bvc :--         ; wait for the next line
            ; switch to display mode 7 (A3 hires) (top scrolling area)
            ; 40 lines come from:
            ; 32nd line down to 77th line down (last 8 are buffer for scroll)
:           lda TEXT        ; display mode 7 (A3 hires) (side view)
            ; set nudge
            ; turn nudging on
            ldx #40         ; stay in mode 7 for 40 lines
:           bit E_IORB
            bvc :-          ; wait until bit 6 (BL) is set
            dex
            beq :++
:           bit E_IORB
            bvs :-          ; wait until bit 6 (BL) is clear again
            bvc :--         ; wait for the next line
            ; switch to display mode 1 (A3 40 text) (main game area)
            ; 48 lines come from:
            ; 400 (chars) 800 (colors), lines (9-15)
:           lda NOMIX
            lda LORES
            ldx #48         ; stay in mode 1 for 48 lines
:           bit E_IORB
            bvc :-          ; wait until bit 6 (BL) is set
            dex
            beq :++
:           bit E_IORB
            bvs :-          ; wait until bit 6 (BL) is clear again
            bvc :--         ; wait for the next line
            ; switch to display mode 7 (A3 hires) (bottom scrolling area)
            ; 40 lines come from:
            ; 
:           lda MIX
            lda HIRES
            ldx #40         ; stay in mode 7 for 40 lines
:           bit E_IORB
            bvc :-          ; wait until bit 6 (BL) is set
            dex
            beq :++
:           bit E_IORB
            bvs :-          ; wait until bit 6 (BL) is clear again
            bvc :--         ; wait for the next line
            ; switch to display mode 1 (A3 40 text) (status area)
:           lda NOMIX
            lda LORES
            ldx #32         ; stay in mode 1 for 32 lines
:           bit E_IORB
            bvc :-          ; wait until bit 6 (BL) is set
            dex
            beq :++
:           bit E_IORB
            bvs :-          ; wait until bit 6 (BL) is clear again
            bvc :--         ; wait for the next line
:                           ; should have run off the bottom now
                            ; stay in mode 1 (A3 40 text) for wrap to the top
            rts

            .include "font.s" ; UploadFont
            .include "lookups.s" ; screen address lookups

varA:       .byte   $C1
linecount:  .byte   0
antinudge:  .byte   0
paintfrom:  .byte   0

nudgecount: .byte   0
IRQSave:    .res    0, 3
Seed:       .byte   0
currMap:    .byte   0       ; line of the map displayed at top of screen

gLevel:     .byte   0
gScore:     .res    0, 3

; start of game
Init:       
            ; save the IRQ vector for restoring upon exit
            lda IRQVECT
            sta IRQSave
            lda IRQVECT + 1
            sta IRQSave + 1
            lda IRQVECT + 2
            sta IRQSave + 2
            lda #$77        ; 2MHz, video, I/O, reset, r/w, ram, ROM#1
            sta EReg            
            ;jsr UploadFont
            jsr MakeField
            lda #$E0        ; start at line E0 of the map, leaves some room down
            sta currMap
            lda #$00        ; start at nudge 0
            sta nudgecount
            jsr PaintScreen
            ; set up VIAs
            ; setup vars and interrupt
            ; put it in display mode 1 where it needs to be at the start of VBL
            lda TEXT
            lda NOMIX
            lda LORES
            lda #$0C
            sta Z_REG       ; put zeropage at page $0C (above screen)
            ; TODO: DO NOT USE ALTERNATE STACK.
            ; THIS WILL MESS UP THE GRAPHICS BECAUSE THE STACK WILL BE NEXT TO ZP
            lda EReg
            and #$FB        ; enable alternate stack
            sta EReg
            ; rts to task 0
            ; Do the update when we are not drawing.
            ; Hoping that we have enough time during VBL for it.
            ; jsr ScrUpdate
            
demo:       ldx #$00        ;display mode
            jsr dispmodex
            lda PAGEONE
            lda SCROLLOFF
            lda KBDCLEAR
            jsr DrawText
            lda #$00
            sta paintfrom
            jsr paint

            lda #$31        ;enable IOCT and keyboard
            sta E_IER

scrollgo:   lda #$07
            sta nudgecount
            lda #$00
            sta antinudge
scrollpos:  lda #$10        ; clear VBLx1 flag
            sta E_IFR
:           lda E_IFR
            and #$10        ; VBL x1?
            beq :-
            ; wait for 24 scanlines to pass
            ; and switch to mode 4
            ldx #24
            jsr waithbl
            ldx #$04
            jsr dispmodex
            lda SCROLLON
            ; wait for 24 scanlines to pass
            ; and switch to mode 5
            ldx #24
            jsr waithbl
            ldx #$05
            jsr dispmodex
            ; wait for 24 scanlines to pass
            ; and switch to mode 1
            ldx #24
            jsr waithbl
            ldx #$01
            jsr dispmodex
            lda SCROLLOFF
            ldx antinudge
            jsr nudgetox

            ; wait for 24 scanlines to pass
            ; and switch to mode 7
            ldx #24
            jsr waithbl
            ldx #$07
            jsr dispmodex
            lda SCROLLON

            ; wait for 24 to pass and switch back to mode 0
            ldx #24
            jsr waithbl
            ldx #$00
            jsr dispmodex
            lda SCROLLOFF
            
;            lda KBD
;            bmi gotkey

            jsr DrawNum

            ; nudge screen during VBL
            ldx nudgecount
            jsr nudgetox
            
            inc antinudge
            dec nudgecount
            bmi sevdone
            
            jmp scrollpos
            
sevdone:    ;inc        varA
            ;jsr        fillx
            ;jsr        paint

            jmp scrollgo
gotkey:     lda SCROLLOFF

            lda #$7f       ;disable all via interrupts
            sta D_IER
            sta D_IFR
            lda #$7f
            sta E_IER
            sta E_IFR

            brk
            .byte   TERMINATE
            .word   *-2

; wait for x (x-register as param) lines to be drawn
waithbl:    bit E_IORB
            bvc waithbl
            ; wait for HBL to go low again
waithblb:   bit E_IORB
            bvs waithblb
            dex
            bne waithbl
waitedhbl:  rts

; write to the text page

DrawText:   lda #$8F
            sta Ptr1+CExtPG
            lda #$04
            sta Ptr1 + 1
            lda #$00
            sta Ptr1
            lda #$A0
            ldy #$27
:           sta (Ptr1),y
            dey
            bpl :-
            inc Ptr1 + 1
            ldy #$27
:           sta (Ptr1),y
            dey
            bpl :-
            lda #$80
            sta Ptr1
            dec Ptr1 + 1
            ldy #$00
:           lda StatText,y
            beq :+
            ora #$80
            sta (Ptr1),y
            iny
            bne :-
:           lda #$A0
            sta (Ptr1),y
            iny
            cpy #$28
            bne :-
            
            lda #$A8
            sta Ptr1
            lda #$05
            sta Ptr1 + 1
            ldy #$00
:           lda ColorText, y
            beq :+
            sta (Ptr1), y
            iny
            bne :-
:           lda #$09
            sta Ptr1 + 1
            ldy #$0F
:           tya
            sta (Ptr1), y
            dey
            bpl :-
            lda #$B8
            sta Ptr1
            ldy #$0F
:           tya
            asl
            asl
            asl
            asl
            sta (Ptr1), y
            dey
            bpl :-
            
            ; FONTCHAR is defined in font.s
DrawNum:    lda #$90
            sta Ptr1
            lda #$04
            sta Ptr1+1
            lda #$8F
            sta Ptr1+CExtPG
            lda #$00
            sta Numbor
            ldy #$05
:           lda ClickNum, y ; base number to display
            tax
            lda FONTCHAR, x ; character this corresponds to
            clc
            adc RollNum, y
            ora Numbor
            sta (Ptr1), y
            lda #$80
            sta Numbor
            dey
            bpl :-
            
IncNum:     ldx #$05
:           inc RollNum,x
            lda RollNum,x
            cmp #$08
            bne IncNumDone
            lda #$00
            sta RollNum,x
            inc ClickNum,x
            lda ClickNum,x
            cmp #$0A
            bne IncNumDone
            lda #$00
            sta ClickNum,x
            dex
            bpl :-
IncNumDone: rts

StatText:   .byte "NUM BER #! ?:"
            .byte $0

ColorText:  .byte "ABCDEFGHIJKLMNOP"
            .byte "ABCDEFGHIJKLMNOP"
            .byte $0

ClickNum:   .byte $0,$0,$0,$0,$0,$0
RollNum:    .byte $0,$0,$0,$0,$0,$0

; HGR graphics is at the beginning of bank 0
paint:      lda #$00
            sta Ptr1+CExtPG
            lda #$00
            sta Ptr1
            sta Ptr1+1
            lda paintfrom
            ldx #$10
            ldy #$00
paintpage:  sta (Ptr1), y
            iny
            bne paintpage
            clc
            adc #$01
            dex
            bne paintpage
            rts

; set display to number in X (clobbers A, Y)

dispmodex:  ldy DMAMAP,x
            lda LC000,y
            ldy DMBMAP,x
            lda LC000,y
            ldy DMCMAP,x
            lda LC000,y
            rts

; set nudge value to X (clobbers A, Y)

nudgetox:   ldy VAIMAP,x
            lda LC000,y
            ldy VBIMAP,x
            lda LC002,y
            ldy VCIMAP,x
            lda LC004,y
            rts

seedRandom:
            ; grab a random number seed from the fastest part of the realtime clock.
            txa
            pha
            lda Z_REG       ; save the ZP register
            tax             ; don't push it just in case we're using alt-stack
            lda #$00
            sta Z_REG       ; request smallest RTC byte
            lda CLOCK       ; close enough to random for now
            sta Seed
            txa
            sta Z_REG       ; restore zero page
            pla             ; restore X
            tax
            rts

; build the playfield representation
; the map is 64 units wide and 256 units tall
; so, that's $40 pages, I'll put it in $2000-$5FFF.
; put it in bank 2 I guess.
; the random numbers aren't going to look very random
; unless I keep reseeding them, so I will.
; This is still deterministic, but at least it should look
; a little better.  This should be improved at some point.
; an individual location on the playfield can be 0 (empty)
; 1 (wall), 2 (another wall), 3 (a third wall).  Should
; be mostly empty space.

MakeField:  lda #$02
            sta Ptr1 + CExtPG
            lda #$00
            sta Ptr1
            lda #$20
            sta Ptr1 + 1
:           jsr seedRandom
            ldx Seed
            ldy #$3F
:           lda Random, x
            and #$07
            sta (Ptr1), y
            inx             ; next random number
            dey             ; next x coordinate
            bne :- 
            lda Ptr1
            clc
            adc #$40
            sta Ptr1
            bcc :--
            inc Ptr1 + 1
            lda Ptr1 + 1
            cmp #$60
            bne :--
            rts

; paint the screen
; at the moment this is doing everything.
; not sure what the cycle count will be, maybe some of this needs to be interleaved.

StatTextA:  .byte "Level: 4  "
            .byte "Score: 012"
            .byte "345       "
            .byte " DISKHERO "
            .byte $0

StatColA:   .byte $D0, $F0, $F0, $F0, $F0, $F0, $F0, $C0, $F0, $F0
            .byte $F0, $F0, $F0, $F0, $F0, $F0, $F0, $A0, $B0, $C0
            .byte $D0, $E0, $90, $F0, $F0, $F0, $F0, $F0, $F0, $F0
            .byte $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E

FrameText:  .byte "XXXXXXXXXX"
            .byte "XXXXXXXXXX"
            .byte "XXXXXXXXXX"
            .byte "XXXXXXXXXX"
            .byte $0

FrameCol:   .byte $D0, $F0, $F0, $F0, $F0, $F0, $F0, $C0, $F0, $F0
            .byte $F0, $F0, $F0, $F0, $F0, $F0, $F0, $A0, $B0, $C0
            .byte $D0, $E0, $90, $F0, $F0, $F0, $F0, $F0, $F0, $F0
            .byte $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E

InnerText:  .byte "XX        "
            .byte "          "
            .byte "          "
            .byte "        XX"
            .byte $0

InnerCol:   .byte $D0, $F0, $F0, $F0, $F0, $F0, $F0, $C0, $F0, $F0
            .byte $F0, $F0, $F0, $F0, $F0, $F0, $F0, $A0, $B0, $C0
            .byte $D0, $E0, $90, $F0, $F0, $F0, $F0, $F0, $F0, $F0
            .byte $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E

ZPSave:     .byte 0
ScrHole     = $78
Zero        = $00

PaintScreen:
            ; paint the static parts of the screen
            ; mode 1 text page
            ; lines 0-1: score status
            ldy #$27
:           lda StatTextA, y
            sta $400, y
            lda StatColA, y
            sta $800, y
            lda StatTextA, y
            sta $480, y
            lda StatColA, y
            sta $880, y
            dey
            bpl :-
            ; lines 20-23: progress status
            ldy #$27
:           lda StatTextA, y
            sta $650, y
            lda StatColA, y
            sta $A50, y
            lda StatTextA, y
            sta $6D0, y
            lda StatColA, y
            sta $AD0, y
            lda StatTextA, y
            sta $750, y
            lda StatColA, y
            sta $B50, y
            lda StatTextA, y
            sta $7D0, y
            lda StatColA, y
            sta $BD0, y
            dey
            bpl :-
            ; lines 9-14: playfield (frame)
            ldy #$27
:           lda FrameText, y
            sta $4A8, y
            sta $728, y
            lda FrameCol, y
            sta $8A8, y ; 
            sta $B28, y ; 
            lda InnerText, y
            sta $528, y
            sta $5A8, y
            sta $628, y
            sta $6A8, y
            lda InnerCol, y
            sta $928, y
            sta $9A8, y
            sta $A28, y
            sta $AA8, y
            dey
            bpl :-
            ; mode 6 super hires
            ; 16 lines, from 16-31.
            ; draw frame for now
            lda #$8F
            sta Ptr1 + CExtPG
            sta Ptr2 + CExtPG
            ldx #$10
:           lda YHiresL, x
            sta Ptr1
            sta Ptr2
            lda YHiresHA, x
            sta Ptr1 + 1
            lda YHiresHB, x
            sta Ptr2 + 1
            ldy #$27
            cpx #$10
            beq mapsolid
            cpx #$1F
            beq mapsolid
            lda #$5A
            ldy #$27
            sta (Ptr1), y
            sta (Ptr2), y
            dey
            lda #$00
:           sta (Ptr1), y
            sta (Ptr2), y
            dey
            bne :-
            lda #$5A
            sta (Ptr1), y
            sta (Ptr2), y
            bne :++
mapsolid:   lda #$7F
:           sta (Ptr1), y
            sta (Ptr2), y
            dey
            bpl :-
:
            ; mode 7 A3 Hires
            ; lines 32-61(+8) and then lines 110-150(+8)
            ; there really isn't anything static here, so
            ; just clear it.
            ldx #$20
            lda YHiresL, x
            sta Ptr1
            sta Ptr2
            lda YHiresHA, x
            sta Ptr1 + 1
            lda YHiresHB, x
            sta Ptr2 + 1
            lda #$00
            ldy #$27
:           sta (Ptr1), y
            sta (Ptr2), y
            dey
            bpl :-
            lda YHiresHC, x
            sta Ptr1 + 1
            lda YHiresHD, x
            sta Ptr2 + 1
            lda #$00
            ldy #$27
:           sta (Ptr1), y
            sta (Ptr2), y
            dey
            bpl :-
            rts

mapPtrL:    .byte   0
mapPtrH:    .byte   0
curLine:    .byte   0
fieldH:     .byte   $04, $05, $05, $06, $06, $07
fieldL:     .byte   $A8, $25, $A8, $28, $A8, $28
fieldHC:    .byte   $08, $09, $09, $0A, $0A, $0B

ScrUpdate:  
            ; save ZP
            lda Z_REG
            sta ZPSave
            ; update the level and score
            ; update level
            lda gLevel
            pha
            and #$F0
            lsr
            lsr
            lsr
            lsr
            ora #$40
            sta $407
            pla
            and #$0F
            ora #$40
            sta $408
            ; update score
            ldy #$03
            ldx #$18
:           lda gScore, y
            pha
            and #$0F
            ora #$40
            sta $480, x
            pla
            lsr
            lsr
            lsr
            lsr
            ora #$40
            dex
            sta $480, x
            dex
            dey
            bpl :-
            ; update progress stats (lines 20-23) TODO
            ; update playfield (lines 9-14)
            ; currMap represents the line at the top of the
            ; playfield
            ; playfield representation starts at 2000 in bank 2.
            ; each line is $40 long
            lda currMap
            ror
            bcc :++
            ; either 1 (40) or 3 (c0)
            lsr
            bcc :+
            ldx #$c0
            bne mapstart
:           ldx #$40
            bne mapstart
            ; either 0 (0) or 2 (80)
:           lsr
            bcc :+
            ldx #$80
            bne mapstart
:           ldx #$00
mapstart:   
            ; we now know the offset into the map data that
            ; we will read from, but we need to store it somewhere
            ; outside the ZP, since we are shifting the ZP around
            stx mapPtrL
            clc
            adc #$20
            sta mapPtrH
            
            lda #$05
            sta curLine
fieldline:  ldx curLine
            lda FieldH, x
            sta Z_REG
            lda FieldL, x
            tax
            
            lda mapPtrL
            sta ScrHole
            lda mapPtrH
            sta ScrHole + 1
            lda #$02
            sta ScrHole + CExtPg
            ; draw characters
            ldy #$27
:           lda (ScrHole), y
            sta Zero, x
            dex
            dey
            bpl :-
            
            ldx curLine
            lda FieldHC, x
            sta Z_REG
            lda FieldL, x
            tax
            ; draw colors
            ldy #$27
:           lda (ScrHole), y
            ora #$88
            sta Zero, x
            dex
            dey
            bpl :-
            
            dec curLine
            bpl fieldline
            
            ; now move to updating tha regions above
            ; and below the playfield.
            ; these are in A3 hires.
            ; the strategy is to ensure that there are lines above and below
            ; so that nudging can work.
            ; only need to redraw entirely when nudging drops below 0 or goes over 7.
            ; this is the impressive part, but I will save it for later TODO
            
            rts
            
CodeEnd     = *