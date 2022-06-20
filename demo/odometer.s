; odometer
; Paul Hagstrom, 2022
; create and upload a font with odometer-style "rolling" numbers

            .segment "CODE"
            .setcpu "6502"

IO_KEY      = $C000     ; keyboard
IO_KEYFLAG  = $C008     ; keyboard flag
IO_KEYCLEAR = $C010     ; keyboard strobe

D_GRAPHICS  = $C050     ; "clear text mode"
D_TEXT      = $C051     ; set text mode
D_NOMIX     = $C052     ; "clear mix mode"
D_MIX       = $C053     ; "set mix mode"
D_PAGEONE   = $C054     ; "clear PG2 mode"
D_PAGETWO   = $C055     ; PG2 mode
D_LORES     = $C056     ; "clear hires mode"
D_HIRES     = $C057     ; set hires mode
SS_XXN      = $C0E0     ; smooth scroll bit 0 off
SS_XXY      = $C0E1     ; smooth scroll bit 0 on
SS_XNX      = $C0E2     ; smooth scroll bit 1 off
SS_XYX      = $C0E3     ; smooth scroll bit 1 on
SS_NXX      = $C0E4     ; smooth scroll bit 2 off
SS_YXX      = $C0E5     ; smooth scroll bit 2 on

TEXTBUFA    = $0400     ; line 0
TEXTBUFB    = $0480     ; line 1
TEXTBUFC    = $0500     ; line 2
TEXTBUFD    = $0580     ; line 3
TEXTBUFE    = $0600     ; line 4
TEXTBUFF    = $0680     ; line 5
GRBUFA      = $2000     ; start of page 1

D_SCROLLON  = $C0D9     ; enable smooth scroll (vertical shift)
D_SCROLLOFF = $C0D8     ; disable smooth scroll

R_TONEHBL   = $FFE0     ; tone (bits 0-5), I/O count (6), slot nmi (7)    
RD_AUXCTL   = $FFDB     ; ffdx auxiliary control register
RE_AUXCTL   = $FFEB     ; ffex auxiliary control register
RD_PERCTL   = $FFDC     ; ffdx peripheral control register
RE_PERCTL   = $FFEC     ; ffex peripheral control register
RD_INTFLAG  = $FFDD     ; ffdx interrupt flag register
RE_INTFLAG  = $FFED     ; ffex interrupt flag register
    ; bit 0=kbd, 1=CA1, 2=shift, 3=CB2 (vblx8), 4=CB1 (vblx1), 5=timer 2, 6=timer 1, 7=IRQ
RD_INTENAB  = $FFDE     ; ffdx interrupt enable register
RE_INTENAB  = $FFEE     ; ffex interrupt enable register
; In III's Company Programming Q&A it was said
; that writing 01 here will disable the keyboard interrupt, 81 re-enables.
; that looks like the reverse of what I wrote above for E_IFR.  Is bit 0 the MSB?

R_ENVIRON   = $FFDF     ; Environment register
R_BANK      = $FFEF     ; Bank register
R_ZP        = $ffd0     ; zero page register
IRQVECT     = $ffcd     ; Monitor IRQ points here

;SOS Calls
;REQUEST_SEG    = $40
;RELEASE_SEG    = $45
TERMINATE   = $65
;OPEN        = $C8
;READ        = $CA
;CLOSE        = $CC

; indirect addressing
XByte       = $1601     ; Interp extended address offset
Ptr         = $20

Numbor      = $22

            .org     $2000 - 14
            
; sos interp header
            .byte    "SOS NTRP"
            .word    0000
            .word    CodeStart
            .word    (CodeEnd-CodeStart)

CodeStart:  jmp init

            .include "odofont.s" ; UploadFont

varA:       .byte   $C1
linecount:  .byte   0
nudgecount: .byte   0
antinudge:  .byte   0
paintfrom:  .byte   0
IRQSave:    .res    0, 3

init:       
            ; [0-------] F000.FFFF RAM (1=RAM)
            ; [-1------] ROM#1 (0=ROM#2)
            ; [--1-----] True stack ($100) (0=alt stack)
            ; [---1----] C000.CFFF read-only (1=read/write)
            ; [----0---] Reset key disabled (1=enabled)
            ; [-----1--] video enabled (0=disabled)
            ; [------1-] C000.CFFF I/O (0=RAM)
            ; [-------1] 2MHz clock (0=1MHz)
            lda #$77        ; 2MHz, video, I/O, reset, r/w, ram, ROM#1
            sta R_ENVIRON
            jsr odofont     ; create and upload odometer font
            lda #$01        ; Apple III color text
            jsr setdisplay
            bit D_PAGEONE
            bit D_SCROLLOFF
            bit IO_KEYCLEAR
            lda #$00
            jsr setnudge
            jsr cleartext
            jsr paintback
            ; show intro screen
            ; initialize graphics
            ; set up VIAs
            ; setup vars and interrupt
            lda #$0C
            sta R_ZP       ; put zeropage at page $0C
            lda R_ENVIRON
            and #$FB        ; enable alternate stack
            sta R_ENVIRON
            ; rts to task 0
            
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

; clear text page (blank page one, F0 colors on page two)

cleartext:  lda #$00
            sta Ptr
            lda #$04
            sta Ptr + 1
            lda #$8F
            sta Ptr + XByte
            ldx #$04
            lda #$A0
            ldy #$00
:           sta (Ptr), y
            iny
            bne :-
            inc Ptr + 1
            dex
            bne :-
            cmp #$F0
            beq :+
            lda #$F0
            ldx #$04
            bne :-
:           rts
            
            
; Set smooth scroll offset to the number (0-7) in A

setnudge:   ror
            bcs snxxy
            bit SS_XXN
            ror
            bcs snxyx
snxnx:      bit SS_XNX
            ror
            bcs sdyxx
snnxx:      bit SS_NXX
            rts
snxxy:      bit SS_XXY
            ror
            bcc ssxnx     
snxyx:      bit SS_XYX
            ror
            bcc ssnxx
snyxx:      bit SS_YXX
            rts

; set display to number in A
; 0 = 40 char Apple II b/w                  gr      nomix   lores
; 1 = 40 char Apple III color               text    nomix   hires
; 2 = 80 char b/w                           gr      mix     lores
; 3 = 80 char b/w                           text    mix     hires
; 4 = Apple II hires (280x192 b/w)          gr      nomix   lores
; 5 = Fg/bg hires (280x192, 16 colors)      text    nomix   hires
; 6 = super hires (560x192, b/w)            gr      mix     lores
; 7 = 140x192 A Hires (140x192, color)      text    mix     hires

setdisplay: ror
            bcs sdtext
            bit D_GRAPHICS
            ror
            bcs sdmix
sdnomix:    bit D_NOMIX
            ror
            bcs sdhires
sdlores:    bit D_LORES
            rts
sdtext:     bit D_TEXT
            ror
            bcc sdnomix     
sdmix:      bit D_MIX
            ror
            bcc sdlores
sdhires:    bit D_HIRES
            rts

CodeEnd     = *