; scroll
; Paul Hagstrom, 2022
; use HBL and VBL interrupts to split graphics modes
; scroll through a map using the smooth scroll parameter

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
IO_CLOCK    = $C070     ; real time clock
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
RE_T2CL     = $FFE8     ; ffdx timer2 count low byte
RE_T2CH     = $FFE9     ; ffdx timer2 count high byte
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
CurrMode:	.byte	0
CurrMap:	.byte	0
NudgeCount:	.byte	0

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
inttimer:   
            inc ScrRegion
            ldx ScrRegion
            ; reset the HBL counter to length of next mode
            lda ScrRegLen, x
            ; living mildly dangerously, assumes that we'll always get
            ; a VBL region reset before we run off the end of the region list
            sta RE_T2CL
            lda #$00
            sta RE_T2CH
            ; move display to correct mode
            lda ScrRegMode, x
            jsr setdisplay
            bne intreturn

; keyboard interrupt handler
; increment count using BCD mode.
intkey:     
			lda IO_KEY
			sta IO_KEYCLEAR
			bpl keyreturn   ; no key pressed, return
			; if the key was E, set the exit flag.
			sta $0400       ; put it in the corner so I can see it
			cmp #$C5        ; E
			bne keyreturn
			inc ExitFlag
keyreturn:  rts

; VBL interrupt handler
intvbl:     
            lda ScrRegLen
			; reset the HBL counter to the length of top region
			sta RE_T2CL
			lda #0
			sta RE_T2CH
            ; reset region number
            sta ScrRegion
			; move display to correct mode
			lda ScrRegMode
			jsr setdisplay
			rts

GameLevel:	.byte	0
GameScore:	.byte	0, 0, 0
ScrRegion:	.byte	0
ScrRegLen:	.byte	$10, $10, $28, $30, $28, $20, $00
ScrRegMode: .byte   $01, $06, $07, $01, $07, $01, $00

init:       
			sei                 ; no interrupts while we are setting up
			;     0------- F000.FFFF RAM (1=RAM)
			;     -1------ ROM#1 (0=ROM#2)
			;     --1----- True stack ($100) (0=alt stack)
			;     ---1---- C000.CFFF read-only (1=read/write)
			;     ----0--- Reset key disabled (1=enabled)
			;     -----1-- video enabled (0=disabled)
			;     ------1- C000.CFFF I/O (0=RAM)
			;     -------1 2MHz clock (0=1MHz)
			lda #%01110111		; 2MHz, video, I/O, reset, r/w, ram, ROM#1
			sta R_ENVIRON
			sta CurrMode
			lda #$01            ; Apple III color text
			jsr setdisplay
			bit D_PAGEONE
			bit D_SCROLLOFF
			bit IO_KEYCLEAR
			lda #$00
			jsr setnudge
			jsr cleartext
			jsr clearhgr
			jsr paintback
			jsr setupenv
			jsr makefield
			lda #$00
			sta ScrRegion
			lda #$80        	; start at line 80 of the map, bottom half
			sta CurrMap
			lda #$00        	; start at nudge 0
			sta NudgeCount
			lda #$00
			sta GameLevel
			lda #$00
			sta GameScore
			cli                 ; all set up now, interrupt away
eventloop:  lda ExitFlag
			bne alldone
            jsr drawscore
            jsr scrupdate
            dec CurrMap
            jmp eventloop
			
alldone:    lda #$7f            ;disable all via interrupts
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
			
			; bank register - $FFEF - E-VIA input register A
			; we will just leave this as-is, which will be the highest
			; available bank given the RAM in the machine.
			
			; ZP register - $FFD0 - D-VIA input/output register B
			; Convention: user $1A, interrupts $00, SOS $18.
			; extended addressing only works from $18-$1F(!)
			; We will still try to point it at graphics pages occasionally.
			; for now, leave it where it is, presumed to be $1A.
			
			; set environment - $FFDF - D-VIA input register A
			; because we will use ZP to draw, we need stack to be true.
			;    [0-------] F000.FFFF RAM (1=ROM)
			;    [-1------] ROM#1 (0=ROM#2)
			;    [--1-----] True stack ($100) (0=alt stack)
			;    [---1----] C000.CFFF read-only (1=read/write)
			;    [----0---] Reset key disabled (1=enabled)
			;    [-----1--] video enabled (0=disabled)
			;    [------1-] C000.CFFF I/O (0=RAM)
			;    [-------1] 2MHz clock (0=1MHz)
			lda #%01110111      ; 2MHz, video, I/O, reset, r/w, ram, ROM#1
			sta R_ENVIRON
			
			; D-VIA
			; register A: environmental register (out)
			; register B: zero page register - and RTC (out)
			; CA1 - global slot IRQ
			; CA2 - sw1, which I think is the open apple key
			; CB1 - serial out, CB2 - serial in.  Usually printer.  Maybe joystick.
			
			; E-VIA
			; register A:
			; [x-------] Any IRQ (in)
			; [-x------] Closed apple key (in)
			; [--x-----] Slot 2 IRQ (in)
			; [---x----] Slot 1 IRQ (in)
			; [-----xxx] Selected bank (out)
			; register B:
			; [--xxxxxx] Sound generator (out)
			; [-x------] I/O count
			; [x-------] nmi in a slot
			; CA1 - RTC interrupt (neg edge active in)
			; CA2 - keyboard interrupt (ind neg edge active) - sets bit 0 of IFR
			; CB1, CB2, shift - VBL
			
			; disable & clear all D-VIA interrupts (MSB=clear, other bits=interrupts)
			; D-VIA Int - flag $FFDD, enable $FFDE
			;     0------- disable
			;     -1------ timer 1
			;     --1----- timer 2
			;     ---1---- CB1
			;     ----1--- CB2
			;     -----1-- shift register
			;     ------1- CA1
			;     -------1 CA2
			lda #%01111111
			sta RD_INTENAB
			sta RD_INTFLAG
			
			; D-VIA Aux control - $FFDB
			;     0------- T1 timer, PB7 disabled
			;     1x------ T1 timer, PB7 one-shot output (10) or square wave output (11)
			;     -0------ T1 timer, timed interrupt each time T1 is loaded (one-shot)
			;     -1------ T1 timer, continueous interrupts
			;     --0----- T2 timer, timed interrupt (1=count down with pulses on PB6)            
			;     ---000-- Shift Reg: disabled
			;     ---100-- Shift Reg: shift out free running at T2 rate
			;     ---1---- Shift Reg: shift out
			;     ---0---- Shift Reg: shift in
			;     ----01-- Shift Reg: under control of T2
			;     ----10-- Shift Reg: under control of O2
			;     ----11-- Shift Reg: under control of ext clock
			;     ------1- PB: Enable latching (0=disable)
			;     -------1 PA: Enable latching (0=disable)
			; D-VIA, no timers enabled            
			lda #%00000000
			sta RD_AUXCTL
			
			; D-VIA - CA1 is any slot IRQ, CA2 is some switch?; CB1, CB2 are SCO/SER, probably joystick?
			; CB2 - [hi nibble: 011-] independent interrupt input pos edge
			; CB1 - [hi nibble: ---1] pos active edge
			; CA2 - [lo nibble: 011-] independent interrupt input pos edge
			; CA1 - [lo nibble: ---0] neg active edge
			; high nibble here is largely irrelevant, ineterrupts CB1, CB@ not used
			; maybe CB1, CB2 relate to joystick.
			lda #%01110110
			sta RD_PERCTL
			
			; disable & clear certain E-VIA interrupts (MSB=clear, other bits=interrupts)
			; not sure why only some are disabled and cleared, this is based on Atomic Defense
			; E-VIA Int -  flag $FFED, enable $FFEE
			;     0------- disable
			;     -0------ timer 1
			;     --0----- timer 2
			;     ---0---- CB1 (VBL)
			;     ----1--- CB2 (VBL)
			;     -----1-- shift register (VBL)
			;     ------1- CA1 (RTC)
			;     -------0 CA2 (keyboard)
			lda #%00001110        ; disable & clear CB2, shift register, CA1
			sta RE_INTENAB
			sta RE_INTFLAG
			
			; E-VIA Aux control - $FFEB
			; [0-------] T1 timer, PB7 disabled
			; [1x------] T1 timer, PB7 one-shot output (10) or square wave output (11)
			; [-0------] T1 timer, timed interrupt each time T1 is loaded (one-shot)
			; [-1------] T1 timer, continueous interrupts
			; [--0-----] T2 timer, timed interrupt (1=count down with pulses on PB6)            
			; [---000--] Shift Reg: disabled
			; [---100--] Shift Reg: shift out free running at T2 rate
			; [---1----] Shift Reg: shift out
			; [---0----] Shift Reg: shift in
			; [----01--] Shift Reg: under control of T2
			; [----10--] Shift Reg: under control of O2
			; [----11--] Shift Reg: under control of ext clock
			; [------1-] PB: Enable latching (0=disable)
			; [-------1] PA: Enable latching (0=disable)
			; 
			; T1 has two latches and a 16 bit counter.  Down to zero -> interrupt
			; one shot keeps counting, free-run resets and counts again
			; T2 can count PB6 negatives, or run in one shot mode.
			; Count: load number to count into T2, dec on pulses, interrupt at zero, counting continues
			; Is PB6 by any chance HBL? Seems likely.
			; E-VIA: enable timer 2, one-shot.  HBL.
			lda #%00100000
			sta RE_AUXCTL
			
			; E-VIA - CA2 is keyboard, CA1 is clock; CB1, CB2 are VBL
			; CB2 011- hi nibble - independent interrupt input pos edge (VBL)
			; CB1 ---0 hi nibble - neg active edge (VBL)
			; CA2     001- lo nibble independent interrupt input neg edge (keyboard)
			; CA1     ---0 lo nibble neg active edge (clock)
			lda #%01100010
			sta RE_PERCTL
			
			; E-VIA Int Enable
			;     1------- enable
			;     -0------ timer 1
			;     --1----- timer 2 (I/O count, HBL)
			;     ---1---- CB1 (VBL)
			;     ----0--- CB2 (VBL)
			;     -----0-- shift register (VBL)
			;     ------0- CA1 (clock)
			;     -------1 CA2 (keyboard)
			; E-VIA - enable timer2 (HBL), CB1 (VBL), CA2 (keyboard)
			lda #%10110001
			sta RE_INTENAB
			
			; E-VIA Int Flag
			;     0------- no function I believe?
			;     -0------ timer 1 
			;     --1----- timer 2 (I/O count, HBL)
			;     ---1---- CB1 (VBL)
			;     ----0--- CB2 (VBL)
			;     -----0-- shift register (VBL)
			;     ------0- CA1 (clock)
			;     -------1 CA2 (keyboard)
			; clear timer2, CB1, CA2
			lda #%00110001
			sta RE_INTFLAG
			
			; set the HBL (E-VIA timer 2) going, kind of guarantees a fast interrupt
			lda #24
			sta RE_T2CL     ;get set
			lda #$00
			sta RE_T2CH     ;go
			rts

; draw the level and score
; this is fast enough we can just do it whenever

drawscore:
            ; update level
            lda GameLevel
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
:           lda GameScore, y
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
            rts

; these are in screen holes because we're using screen memory for ZP
ScrHole     = $78
OtherZP     = $7A
LineStart   = $7C
CurrMapX    = $7D
ScrHoleD    = $7B
MapBuffer   = $F8
PixScratch  = $FF
Zero        = $00
DrawLine:   .byte   0
CurMapLine: .byte   0
MapPtrL:    .byte   0
MapPtrH:    .byte   0
FieldH:     .byte   $04, $05, $05, $06, $06, $07
FieldL:     .byte   $A8, $25, $A8, $28, $A8, $28
FieldHC:    .byte   $08, $09, $09, $0A, $0A, $0B
MapColors:  .byte   $00, $0C, $0D, $0E, $04

scrupdate:

            ; update playfield (lines 9-14)
            ; currMap represents the line at the top of the
            ; playfield
            ; playfield representation starts at 2000 in bank 2.
            ; each line is $40 long
            ; TODO: This should really also take into account an
            ; x-coordinate, so that we can move the window left
            ; and right. We're displaying only 40-ish (36?)
            ; of the 64 dots in the main playfield.
            ; also TODO: this will at some point get custom fonts
            ; and colors, at which point we need to know which
            ; character variant is appropriate. Animation frame?
            ; facing direction?
            
            ; get pointer into the map data for current row
            lda CurrMap
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
            stx MapPtrL
            clc
            adc #$20
            sta MapPtrH
            
            ; the pointer is to the TOP of the whole screen
            ; the whole screen is 80 (28 30 28) long
            ; first mode 7 stuff is from 0 to 28.  Drawn from line $20
            ; the mode 1 stuff is 28 lines down.  Drawn from line $09
            ; and 6 lines long (magnified by 8 vertically)
            ; it starts at 28+18-3 = 3D
            ; and goes to 42.
            ; then the next mode 7 stuff is from 58 to 80.  Drawn from line $78.
            
            lda R_ZP
            sta ZPSave

            ; top mode 7 part

            lda #$20        ; starts at line $20
            sta DrawLine
fieldline:  ldx DrawLine
            lda YHiresHZA, x
            ; we will use ZP to write to graphics memory, but there are two
            ; graphics pages involved.
            ; engineer it so that OtherZP always points to the other ZP to flip quickly.
            sta R_ZP
            pha
            clc
            adc #$20
            sta OtherZP
            sta R_ZP
            pla
            sta OtherZP
            sta R_ZP
            ; lo byte is the same on either page
            lda YHiresL, x
            sta LineStart
            ; Zero, LineStart is now the left side of the draw line in graphics memory
            ; because this is ZP-dependent, we need to se the map pointer up only once we've set ZP.
            lda MapPtrL
            sta ScrHole
            lda MapPtrH
            sta ScrHole + 1
            lda #$02
            sta ScrHole + XByte
            ; (ScrHole), 0 is now the left side of the map line
            ; in mode 7 we draw the whole thing, start at the right
            ; it is a 140-pixel space, and we have a 64-byte wide thing to draw
            ; so we want to double up pixels (unless we expand it to be a 128 byte wide thing)
            ; so we read 7 map elements and push them out into 8 bytes
            ; start at the right edge 
topline:    lda #$27
            sta CurMapX
            ; pixels are in groups of 7 split over 4 bytes
            ; load the seven map elements we will represent
toplineseg: ldy CurMapX
            ldx #$06
:           lda (ScrHole), y
            sta MapBuffer, x
            dey
            dex
            bpl :-
            sty CurMapX
            ; blast them onto the screen via our handy ZP
            ; byte 0 (page 1): -000011 [0+0]
            ldx LineStart
            ldy MapBuffer
            lda MapColors, y
            tay
            lsr
            php                 ; save the last bit in carry
            sta PixScratch
            tya                 ; same color, horizontally doubled
            asl
            asl
            asl
            ora PixScratch
            sta Zero, x
            ; byte 0 (page 2): -1222233 [0+1+1]
            ldy MapBuffer + 1
            lda MapColors, y
            asl
            asl
            plp
            bcc :+
            ora #$40
:           sta PixScratch
            lda MapColors, y    ; same color, horizontally doubled
            lsr
            php
            lsr
            php
            ora PixScratch
            ; put this data on the other ZP
            ldy OtherZP
            sty R_ZP
            sta Zero, x
            ldy OtherZP
            sty R_ZP
            inx
            ; byte 1 (page 1): -3344445 [1+2+2]
            ; we stashed the lower pixel 3 bits on the stack as carries
            ldy MapBuffer + 2
            lda MapColors, y
            clc
            and #$08
            beq :+
            sec
:           rol
            plp
            bcc :+
            ora #$40
:           plp
            bcc :+
            ora #$20
:           sta Zero, x
            ; byte 1 (page 2): -5556666 [2+3]
            lda MapColors, y    ; same color, horizontally doubled
            asl
            asl
            asl
            asl
            and #$7F
            sta PixScratch
            ldy MapBuffer + 3
            lda MapColors, y
            ora PixScratch
            ; put this data on the other ZP
            ldy OtherZP
            sty R_ZP
            sta Zero, x
            ldy OtherZP
            sty R_ZP
            inx
            ; we do it again because the doubling crosses two groups
            ; byte 2 (page 1): -7777888 [3+4]
            ldy MapBuffer + 4
            lda MapColors, y
            lsr
            php
            sta PixScratch
            ldy MapBuffer + 3
            lda MapColors, y
            asl
            asl
            asl
            ora PixScratch 
            sta Zero, x
            ; byte 2 (page 2): -89999AA [4+4+5]
            ldy MapBuffer + 4
            lda MapColors, y
            asl
            asl
            plp
            bcc :+
            ora #$40
:           sta PixScratch
            lda MapBuffer + 5
            lda MapColors, y
            lsr
            php
            lsr
            php
            ora PixScratch
            ; put this data on the other ZP
            ldy OtherZP
            sty R_ZP
            sta Zero, x
            ldy OtherZP
            sty R_ZP
            inx
            ; byte 3 (page 1): -AABBBBC [5+5+6]
            ldy MapBuffer + 6
            lda MapColors, y
            ror
            ldy MapBuffer + 5
            lda MapColors, y
            rol
            plp
            bcc :+
            ora #$40
:           plp
            bcc :+
            ora #$20
:           sta Zero, x
            ; byte 3 (page 2): -CCCDDDD [6+6]
            ldy MapBuffer + 6
            lda MapColors, y    ; same color, horizontally doubled
            sta PixScratch
            asl
            asl
            asl
            asl
            and #$7F
            ora PixScratch
            ; put this data on the other ZP
            ldy OtherZP
            sty R_ZP
            sta Zero, x
            ldy OtherZP
            sty R_ZP
            
            ; the 14 pixels are now drawn
            ; continue back the line
            ; the map pointer was already decremented when we buffered it.
            lda CurrMapX
            bmi :+          ; already done
            jmp toplineseg
            
            ; and the line is now drawn
            ; advance the map pointer
            lda MapPtrL
            adc #$40
            sta MapPtrL
            bcc :+
            inc MapPtrH
:            
            ; advance the graphics line
            inc DrawLine
            lda DrawLine
            cmp #$48
            beq :+
            jmp fieldline
:            
            ; do the above again but for the lower field
            ; that is from 58 to 80.  Drawn from line $78.
            
            ; then do the mode 1 stuff
            ;YOU ARE HERE
            
            ldy #$27
:           lda (ScrHole), y
            sta Zero, x
            dex
            dey
            bpl :-
            
            ldx CurMapLine
            lda FieldHC, x
            sta R_ZP
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
            
            dec CurMapLine
            bpl fieldline
                        
            lda #$05
            sta CurMapLine
fieldline:  ldx CurMapLine
            lda FieldH, x
            sta R_ZP
            lda FieldL, x
            tax
            
            lda MapPtrL
            sta ScrHole
            lda MapPtrH
            sta ScrHole + 1
            lda #$02
            sta ScrHole + XByte
            ; draw characters
            ldy #$27
:           lda (ScrHole), y
            sta Zero, x
            dex
            dey
            bpl :-
            
            ldx CurMapLine
            lda FieldHC, x
            sta R_ZP
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
            
            dec CurMapLine
            bpl fieldline
            
            ; now move to updating tha regions above
            ; and below the playfield.
            ; these are in A3 hires.
            ; the strategy is to ensure that there are lines above and below
            ; so that nudging can work.
            ; only need to redraw entirely when nudging drops below 0 or goes over 7.
            ; this is the impressive part, but I will save it for later TODO
            
            ; however: plan.  The map is 64 bytes wide. We have 140 pixels.
            ; so we can draw two pixels per column.  Might have been better
            ; to have had 128 bytes wide.
            
            
            
            lda ZPSave
            sta R_ZP
            rts
            
Seed:		.byte	0

seedRandom:
			; grab a random number seed from the fastest part of the realtime clock.
			txa
			pha
			lda R_ZP       	; save the ZP register
			tax             ; don't push it just in case we're using alt-stack
			lda #$00
			sta R_ZP       	; request smallest RTC byte
			lda IO_CLOCK	; close enough to random for now
			sta Seed
			txa
			sta R_ZP		; restore zero page
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

makefield:  lda #$02
			sta PtrA + XByte
			lda #$00
			sta PtrA
			lda #$20
			sta PtrA + 1
:           jsr seedRandom
			ldx Seed
			ldy #$3F
:           lda Random, x
			and #$07
			sta (PtrA), y
			inx             ; next random number
			dey             ; next x coordinate
			bne :- 
			lda PtrA
			clc
			adc #$40
			sta PtrA
			bcc :--
			inc PtrA + 1
			lda PtrA + 1
			cmp #$60
			bne :--
			rts

; paint the static parts of the page

StatTextA:  .byte "Level:    "
			.byte "Score:    "
			.byte "         S"
			.byte "CROLLDEMO "
			.byte $0

StatColA:   .byte $D0, $F0, $F0, $F0, $F0, $F0, $F0, $C0, $F0, $F0
			.byte $F0, $F0, $F0, $F0, $F0, $F0, $F0, $A0, $B0, $C0
			.byte $D0, $E0, $90, $F0, $F0, $F0, $F0, $F0, $0E, $0E
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

paintback:  
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
			sta PtrA + XByte
			sta PtrB + XByte
			ldx #$1F
 mapline:   lda YHiresL, x
			sta PtrA
			sta PtrB
			lda YHiresHA, x
			sta PtrA + 1
			lda YHiresHB, x
			sta PtrB + 1
			ldy #$27
			cpx #$10
			beq mapsolid
			cpx #$1F
			beq mapsolid
			lda #$5A
			ldy #$27
			sta (PtrA), y
			sta (PtrB), y
			dey
			lda #$00
:           sta (PtrA), y
			sta (PtrB), y
			dey
			bne :-
			lda #$5A
			sta (PtrA), y
			sta (PtrB), y
			bne :++
mapsolid:   lda #$7F
:           sta (PtrA), y
			sta (PtrB), y
			dey
			bpl :-
:           dex
            bpl mapline
            
			; mode 7 A3 Hires
			; lines 32-61(+8) and then lines 110-150(+8)
			; there really isn't anything static here, so
			; just clear it.
			ldx #$20
			lda YHiresL, x
			sta PtrA
			sta PtrB
			lda YHiresHA, x
			sta PtrA + 1
			lda YHiresHB, x
			sta PtrB + 1
			lda #$00
			ldy #$27
:           sta (PtrA), y
			sta (PtrB), y
			dey
			bpl :-
			lda YHiresHC, x
			sta PtrA + 1
			lda YHiresHD, x
			sta PtrB + 1
			lda #$00
			ldy #$27
:           sta (PtrA), y
			sta (PtrB), y
			dey
			bpl :-
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

; clear graphics pages (just fill with nonsense for now)

clearhgr:	lda #$00
			sta PtrA
			lda #$20
			sta PtrA + 1
			lda #$8F
			sta PtrA + XByte
			ldx #$80
			lda #$40
			ldy #$00
:			sta (PtrA), y
			iny
			bne :-
			inc PtrA + 1
			dex
			bne :-
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

			.include "lookups.s"

CodeEnd     = *