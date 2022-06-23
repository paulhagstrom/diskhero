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
R_ZP        = $FFD0     ; zero page register
IRQVECT     = $FFCD     ; Monitor IRQ points here

;SOS Calls
;REQUEST_SEG    = $40
;RELEASE_SEG    = $45
TERMINATE   = $65
;OPEN        = $C8
;READ        = $CA
;CLOSE        = $CC

; indirect addressing
XByte       = $1601     ; Interp extended address offset
PtrA        = $20
PtrB        = $22
PtrC        = $24
PtrS        = $26
PrColor     = $28

            .org     $2000 - 14
            
; sos interp header
            .byte    "SOS NTRP"
            .word    0000
            .word    CodeStart
            .word    (CodeEnd-CodeStart)

CodeStart:  jmp init

ExitFlag:   .byte   0
IRQSave:    .byte   0, 0 , 0

inthandler: ; save registers
            pha
            tya
            pha
            txa
            pha
            cld
            lda RE_INTFLAG
            and #$10        ; CB1 (VBL)
            beq :+          ; not VBL
            jsr intvbl
            lda #$10        ; clear CB1 VBL
            sta RE_INTFLAG
            bne intreturn
:           lda RE_INTFLAG
            and #$01        ; CA2 (Keyboard)
            beq :+          ; not keyboard
            jsr intkey 
            lda #$01        ; clear CA2 keyboard
            sta RE_INTFLAG
            bne intreturn
:           lda RE_INTFLAG
            and #$20        ; timer 2
            bne inttimer
intreturn:  pla
            tax
            pla
            tay
            pla
            rti
; timer2 interrupt handler
inttimer:   lda #$20        ;clear timer2 flag
            sta RE_INTFLAG
            dec CountHBL + 3
            bpl timerdo
            lda #$07
            sta CountHBL + 3
            lda CountHBL + 2
            sed
            clc
            adc #$01
            sta CountHBL + 2
            bcc timerdo
            lda CountHBL + 1
            adc #$00
            sta CountHBL + 1
            bcc timerdo
            lda CountHBL
            adc #$00
            sta CountHBL
timerdo:    cld
            clc
            bcc intreturn
            
; keyboard interrupt handler
; increment count using BCD mode.
intkey:     
            lda IO_KEY
            sta IO_KEYCLEAR
            bpl keyreturn   ; no key pressed, return
            ; if the key was E, set the exit flag.
            sta $0400       ; put it in the corner so I can see it
            cmp #$C5        ; E
            bne inckey
            inc ExitFlag
            ; increment key event counter
inckey:     dec CountKBD + 3
            bpl keyreturn
            lda #$07
            sta CountKBD + 3
            lda CountKBD + 2
            sed
            clc
            adc #$01
            sta CountKBD + 2
            bcc keyreturn
            lda CountKBD + 1
            adc #$00
            sta CountKBD + 1
            bcc keyreturn
            lda CountKBD
            adc #$00
            sta CountKBD
keyreturn:  cld
            rts

; VBL interrupt handler
; this one plays two roles.  One: it counts VBLs.  Two: it draw the numbers.
intvbl:     
            dec CountVBL + 3
            bpl vbldraw
            lda #$07
            sta CountVBL + 3
            lda CountVBL + 2
            sed
            clc
            adc #$01
            sta CountVBL + 2
            bcc vbldraw
            lda CountVBL + 1
            adc #$00
            sta CountVBL + 1
            bcc vbldraw
            lda CountVBL
            adc #$00
            sta CountVBL
vbldraw:    cld
            jsr drawnums
            rts

init:       
            sei                 ; no interrupts while we are setting up
            ; [0-------] F000.FFFF RAM (1=RAM)
            ; [-1------] ROM#1 (0=ROM#2)
            ; [--1-----] True stack ($100) (0=alt stack)
            ; [---1----] C000.CFFF read-only (1=read/write)
            ; [----0---] Reset key disabled (1=enabled)
            ; [-----1--] video enabled (0=disabled)
            ; [------1-] C000.CFFF I/O (0=RAM)
            ; [-------1] 2MHz clock (0=1MHz)
            lda #$77            ; 2MHz, video, I/O, reset, r/w, ram, ROM#1
            sta R_ENVIRON
            jsr odofont         ; create and upload odometer font
            lda #$01            ; Apple III color text
            jsr setdisplay
            bit D_PAGEONE
            bit D_SCROLLOFF
            bit IO_KEYCLEAR
            lda #$00
            jsr setnudge
            jsr cleartext
            jsr paintback
            jsr drawnums
            jsr setupenv
            cli                 ; all set up now, interrupt away
eventloop:  lda ExitFlag
            beq eventloop
            
            lda #$7f            ;disable all via interrupts
            sta RD_INTENAB
            sta RD_INTFLAG
            lda #$7f
            sta RE_INTENAB
            sta RE_INTFLAG

            brk
            .byte   TERMINATE
            .word   *-2

; arm interrupts

setupenv:   ; save IRQ vector and then install ours
            lda IRQVECT
            sta IRQSave
            lda IRQVECT + 1
            sta IRQSave + 1
            lda IRQVECT + 2
            sta IRQSave + 2
            lda #$4C        ; jmp
            sta IRQVECT
            lda #<inthandler
            sta IRQVECT + 1
            lda #>inthandler
            sta IRQVECT + 2
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
            lda #$7F        ; disable & clear all interrupts (MSB=clear, other bits=interrupts)
            sta RD_INTENAB
            sta RD_INTFLAG
            ; CB2, CA1, shift register handle VBL behavior.
            ; E-VIA Int Flag
            ; [0-------] disable
            ; [-0------] timer 1
            ; [--0-----] timer 2
            ; [---0----] CB1
            ; [----1---] CB2
            ; [-----1--] shift register
            ; [------1-] CA1
            ; [-------0] CA2
            lda #$0E        ; disable & clear CB2, shift register, CA1
            sta RE_INTENAB
            sta RE_INTFLAG
            ; D-VIA Aux control:
            ; [---000--] disabled
            ; other options (maybe copy somewhere else)
            ; [---000--] disabled
            ; [---100--] shift out free running at T2 rate
            ; [---1----] shift out
            ; [---0----] shift in
            ; [----01--] under control of T2
            ; [----10--] under control of +2 (?)
            ; [----11--] under control of ext clock
            ; T1 has two latches and a 16 bit counter.  Down to zero -> interrupt
            ; one shot keeps counting, free-run resets and counts again
            ; T2 can count PB6 negatives, or run in one shot mode.
            ; Count: load number to count into T2, dec on pulses, interrupt at zero, counting continues
            ; Is PB6 by any chance HBL? 
            lda #$00        ; timed interrupt
            sta RD_AUXCTL
            sta RE_AUXCTL
            ; E-VIA - CA2 is keyboard, CA1 is clock; CB1, CB2 are VBL
            ; CB2 - [hi nibble: 011-] independent interrupt input pos edge
            ; CB1 - [hi nibble: ---0] neg active edge
            ; CA2 - [lo nibble: 001-] independent interrupt input neg edge
            ; CA1 - [lo nibble: ---0] neg active edge
            lda #$62
            sta RE_PERCTL
            ; D-VIA - CA1 is any slot IRQ, CA2 is some switch?; CB1, CB2 are SCO/SER, probably joystick?
            ; CB2 - [hi nibble: 011-] independent interrupt input pos edge
            ; CB1 - [hi nibble: ---1] pos active edge
            ; CA2 - [lo nibble: 011-] independent interrupt input pos edge
            ; CA1 - [lo nibble: ---0] neg active edge
            lda #$76
            sta RD_PERCTL
            ; E-VIA Int Enable
            ; [1-------] enable
            ; [-1------] timer 1
            ; [--1-----] timer 2
            ; [---1----] CB1
            ; [----0---] CB2
            ; [-----0--] shift register
            ; [------0-] CA1
            ; [-------1] CA2
            lda #$F1        ; enable timer1, timer2, CB1, CA2
            sta RE_INTENAB
            ; E-VIA Int Flag
            ; [0-------] no function I believe?
            ; [-1------] timer 1
            ; [--1-----] timer 2
            ; [---1----] CB1
            ; [----0---] CB2
            ; [-----0--] shift register
            ; [------0-] CA1
            ; [-------1] CA2
            lda #$71        ; clear timer1, timer2, CB1, CA2
            sta RE_INTFLAG
            rts

; draw a single number

DigitList:  .byte 0, 0, 0, 0, 0, 0
CharList:   .byte 0, 0, 0, 0, 0, 0

drawnum:    tya
            pha                 ; stash column number for later
            ldy #$00
:           ldx #$00            ; some acrobatics to avoid extended addressing mode
            lda (PtrC, x)
            pha
            lsr
            lsr
            lsr
            lsr
            sta DigitList, y
            tax
            lda FontChar, x
            ora #$80            ; normal not inverse
            sta CharList, y
            iny
            pla
            and #$0F
            sta DigitList, y
            tax
            lda FontChar, x
            ora #$80            ; normal not inverse
            sta CharList, y
            inc PtrC
            bne :+
            inc PtrC + 1
:           iny
            cpy #$06
            bne :--
            ; base digits now in place
            ; last "digit" is base 8, determines the "roll" step
            ; and not binary coded decimal
            ; PtrC now points there.
            ; roll should actually be 7 minus stored value
            ; because it's decremented for efficiency elsewhere
            ldx #$00
            lda #$07
            sbc (PtrC, x)            
            tax                 ; stash roll offset in X for use later
            dey                 ; point to final digit
            clc
            adc CharList, y
            and #$7F            ; make the last one inverse
            sta CharList, y     ; and roll
            ; if digit we are rolling is a 9, roll the one before it too
:           lda DigitList, y
            cmp #$09
            bne dndone          ; advqnced the last roll-adjacent 9
            dey
            bmi dndone          ; no more digits
            txa                 ; restore roll offset from X
            clc                 ; roll the adjacent digit
            adc CharList, y
            sta CharList, y
            bne :-              ; back to check more digits
dndone:     ; we now have all the digits and rolls determined, put them onscreen
            pla                 ; pull column number
            clc
            adc #$05
            tay
            ldx #$05
:           lda CharList, x
            sta (PtrA), y
            dey
            dex
            bpl :-
            rts

            ; FontChar is defined in odofont.s
drawnums:   
            ldx #$08
            jsr getlinex        ; load PtrA (text), PtrB (color) for line
            lda #<CountVBL      ; load number to draw into PtrC
            sta PtrC
            lda #>CountVBL
            sta PtrC + 1
            ldy #22             ; column to draw numbers at.
            jsr drawnum
            
            ldx #$0A
            jsr getlinex        ; load PtrA (text), PtrB (color) for line
            lda #<CountKBD      ; load number to draw into PtrC
            sta PtrC
            lda #>CountKBD
            sta PtrC + 1
            ldy #22             ; column to draw numbers at.
            jsr drawnum
            
            ldx #$0C
            jsr getlinex        ; load PtrA (text), PtrB (color) for line
            lda #<CountHBL      ; load number to draw into PtrC
            sta PtrC
            lda #>CountHBL
            sta PtrC + 1
            ldy #22             ; column to draw numbers at.
            jsr drawnum
            rts
            
CountVBL:   .byte 0, 0, 0, 7
CountKBD:   .byte 0, 0, 0, 7
CountHBL:   .byte 0, 0, 0, 7

TextOne:    .byte "INTERRUPT COUNTING", 0
TextTwo:    .byte "VBL INTERRUPTS:", 0
TextThree:  .byte "KEYBOARD INTERRUPTS:", 0
TextFour:   .byte "HBL INTERRUPTS:", 0

; paint the static parts of the page

paintback:  
            ldx #$04        ; vtab
            jsr getlinex
            lda #>TextOne
            pha
            lda #<TextOne
            pha
            lda #$E0        ; color
            pha
            jsr printstr
            ldx #$08        ; vtab
            jsr getlinex
            lda #>TextTwo
            pha
            lda #<TextTwo
            pha
            lda #$C0        ; color
            pha
            jsr printstr
            ldx #$0A        ; vtab
            jsr getlinex
            lda #>TextThree
            pha
            lda #<TextThree
            pha
            lda #$0E        ; color
            pha
            jsr printstr
            ldx #$0C        ; vtab
            jsr getlinex
            lda #>TextFour
            pha
            lda #<TextFour
            pha
            lda #$0C        ; color
            pha
            jsr printstr
            rts            

; print the characters that we were passed on the stack, to PtrA/PtrB (found via getlinex)
; push string high, string low, color, then call

printstr:
            ; stash return address
            pla
            sta PtrS
            pla
            sta PtrS + 1
            ; get string 
            pla
            sta PrColor
            pla
            sta PtrC
            pla
            sta PtrC + 1
            ; replace return address
            lda PtrS + 1
            pha
            lda PtrS
            pha
            ; 
            ldx #$00
            ldy #$00
:           lda (PtrC, x)
            beq :++
            ora #$80
            sta (PtrA), y
            inc PtrC
            bne :+
            inc PtrC + 1
:           iny
            bne :--
:           lda PrColor
:           sta (PtrB), y
            dey
            bne :-
            sta (PtrB), y
            rts
        
; put the pointers to line x into PtrA (char) and PtrB (color)

getlinex:
            lda YLoresL, x
            sta PtrA
            sta PtrB
            lda YLoresHA, x
            sta PtrA + 1
            lda YLoresHB, x
            sta PtrB + 1
            lda #$8F
            sta PtrA + XByte
            sta PtrB + XByte
            rts

; clear text page (blank page one, F0 colors on page two)

cleartext:  lda #$00
            sta PtrA
            lda #$04
            sta PtrA + 1
            lda #$8F
            sta PtrA + XByte
            ldx #$04
            lda #$A0
            ldy #$00
:           sta (PtrA), y
            iny
            bne :-
            inc PtrA + 1
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
            bcs snyxx
snnxx:      bit SS_NXX
            rts
snxxy:      bit SS_XXY
            ror
            bcc snxnx     
snxyx:      bit SS_XYX
            ror
            bcc snnxx
snyxx:      bit SS_YXX
            rts

; set display to number in A
; 0 = 40 char Apple II b/w                  gr      nomix   lores
; 1 = 40 char Apple III color               text    nomix   lores
; 2 = 80 char b/w                           gr      mix     lores
; 3 = 80 char b/w                           text    mix     lores
; 4 = Apple II hires (280x192 b/w)          gr      nomix   hires
; 5 = Fg/bg hires (280x192, 16 colors)      text    nomix   hires
; 6 = super hires (560x192, b/w)            gr      mix     hires
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

YLoresL:
            .byte $00, $80, $00, $80, $00, $80, $00, $80
            .byte $28, $A8, $28, $A8, $28, $A8, $28, $A8
            .byte $50, $D0, $50, $D0, $50, $D0, $50, $D0

YLoresHA:
            .byte $04, $04, $05, $05, $06, $06, $07, $07
            .byte $04, $04, $05, $05, $06, $06, $07, $07
            .byte $04, $04, $05, $05, $06, $06, $07, $07

YLoresHB:
            .byte $08, $08, $09, $09, $0A, $0A, $0B, $0B
            .byte $08, $08, $09, $09, $0A, $0A, $0B, $0B
            .byte $08, $08, $09, $09, $0A, $0A, $0B, $0B
            
            .include "odofont.s" ; odofont
            
CodeEnd     = *