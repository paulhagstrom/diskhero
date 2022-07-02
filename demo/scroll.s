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
ZPtrA       = $20
ZPtrB       = $22
ZPtrC       = $24
ZPtrD       = $26

			.org     $A000 - 14
			
; sos interp header
            .byte    "SOS NTRP"
            .word    0000
            .word    CodeStart
            .word    (CodeEnd-CodeStart)

CodeStart:  jmp init

IRQSave:    .byte   0, 0 , 0        ; saved state
ZPSave:     .byte   0
BankSave:   .byte   0

ExitFlag:   .byte   0               ; keyboard int mskes this nonzero to triggger exit
CurrMode:	.byte	0
CurrMap:	.byte	0
NudgeCount:	.byte	0

GameLevel:	.byte	0
GameScore:	.byte	0, 0, 0
ScrRegion:	.byte	0
FieldH:     .byte   $04, $05, $05, $06, $06, $07
FieldL:     .byte   $A8, $25, $A8, $28, $A8, $28
FieldHC:    .byte   $08, $09, $09, $0A, $0A, $0B
MapColors:  .byte   $00, $CC, $DD, $EE, $44
PlayColors: .byte   $00, $1C, $2D, $3E, $C4
PlayChars:  .byte   $41, $C2, $43, $21, $C7
ScrRegLen:	.byte	$0E, $0F, $27, $2F, $27, $1F, $00
ScrRegMode: .byte   $01, $06, $07, $01, $07, $01, $00
; I played with ScRegLen by trial and error a little.
; Not sure why I needed to go two down for region zero.

; these are in screen holes because we're using screen memory for ZP
ZScrHole    = $78
ZOtherZP    = $7A
ZCurrDrawX  = $7B
ZLineStart  = $7C
ZCurrMapX   = $7E
ZMapBuffer  = $F8
ZPxScratch  = $FF
Zero        = $00
CurrLine:   .byte   0
CurMapLine: .byte   0
MapPtrL:    .byte   0
MapPtrH:    .byte   0
StashZP:    .byte   0

; we need to prioritize interrupts a bit because they can pile up.
; if HBL has priority we can wind up in an situation where nothingg
; else gets a chance to fire.  So keyboard first, then VBL, then HBL/timer.

inthandler: ; save registers
            pha
            tya
            pha
            txa
            pha
            cld
            lda RE_INTFLAG
            and #$10        ; VBL?
            beq :+          ; nope
            jsr intvbl
            lda #$10        ; clear CB1 VBL
            sta RE_INTFLAG
            bne intreturn
:           lda RE_INTFLAG
            and #$01        ; keyboard?
            beq :+          ; not $01 CA2 keyboard
            jsr intkey
            lda #$01        ; clear CA2 keyboard
            sta RE_INTFLAG
            bne intreturn
:           lda RE_INTFLAG
            and #$20        ; check for $20 (timer/HBL)
            bne inttimer
intreturn:  pla
            tax
            pla
            tay
            pla
            rti
; timer2 (HBL) interrupt handler
inttimer:   
            inc $0402
            inc ScrRegion
            ldx ScrRegion
            ; move display to correct mode
            lda ScrRegMode, x
            jsr setdisplay
            ; reset the HBL counter to length of next mode
            lda ScrRegLen, x
            ; living mildly dangerously, assumes that we'll always get
            ; a VBL region reset before we run off the end of the region list
            sta RE_T2CL
            lda #$00
            sta RE_T2CH
            ; clear the timer2 flag (resetting the timer clock seems not to do it?)
            lda #$20
            sta RE_INTFLAG
            bne intreturn

; keyboard interrupt handler
; increment count using BCD mode.
intkey:     
            lda IO_KEY
            sta IO_KEYCLEAR
            bpl keyreturn   ; no key pressed, return
            ; if the key was E, set the exit flag.
            sta $0400       ; put it in the corner so I can see it
            dec CurrMap     ; move, for now.
            cmp #$C5        ; E
            bne keyreturn
            inc ExitFlag
keyreturn:  rts

; VBL interrupt handler
intvbl:     
            inc $0401
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

init:       
			sei                 ; no interrupts while we are setting up
			;     0------- 2MHz clock (1=1MHz)
			;     -1------ C000.CFFF I/O (0=RAM)
			;     --1----- video enabled (0=disabled)
			;     ---1---- Reset key enabled (0=disabled)
			;     ----0--- C000.CFFF read/write (1=read only)
			;     -----1-- True stack ($100) (0=alt stack)
			;     ------1- ROM#1 (0=ROM#2)
			;     -------1 F000.FFFF RAM (1=ROM)
			lda #%01110111		; 2MHz, video, I/O, reset, r/w, ram, ROM#1, true stack
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
            inc GameScore + 2
            ;dec CurrMap
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
            ;     0------- 2MHz clock (1=1MHz)
            ;     -1------ C000.CFFF I/O (0=RAM)
            ;     --1----- video enabled (0=disabled)
            ;     ---1---- Reset key enabled (0=disabled)
            ;     ----0--- C000.CFFF read/write (1=read only)
            ;     -----1-- True stack ($100) (0=alt stack)
            ;     ------1- ROM#1 (0=ROM#2)
            ;     -------1 F000.FFFF RAM (1=ROM)
			lda #%01110111      ; 2MHz, video, I/O, reset, r/w, ram, ROM#1, true stack
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
            ora #$30
            sta $407
            pla
            and #$0F
            ora #$30
            sta $408
            ; update score
            ldy #$03
            ldx #$18
:           lda GameScore, y
            pha
            and #$0F
            ora #$30
            sta $480, x
            pla
            lsr
            lsr
            lsr
            lsr
            ora #$30
            dex
            sta $480, x
            dex
            dey
            bpl :-
            rts

scrupdate:
            ; update playfield (lines 9-14)
            ; CurrMap represents the line in the map data at top of playfield
            ; playfield representation starts at 2000 in bank 2, each line is $40 long
            ; so lines at 2000, 2040, 2080, 20C0, 2100, 2140, etc.
            ; anything interacting with the map should activate $1C00 ZP
            ; we will temporarily switch to graphics-based ZP when necessary
            lda R_ZP
            sta ZPSave          ; save current ZP
            lda R_BANK
            sta BankSave        ; save current bank
            lda #$1C
            sta R_ZP            ; switch ZP to $1C00
            lda #$00            ; swap in bank zero,
            sta R_BANK          ; where (hires) graphics memory lives
            ; translate pointer to top displayed line in map (CurMap)
            ; to an address for its data (in bank 1 at $1000).
            ; If CurrMap is something like 00000111 (7), shift bits to translate to:
            ; MapPtrL: 11000000 (C0) MapPtrH: 00010001 (11) ($11C0 and $40 bytes there)
            lda #$00
            sta MapPtrL
            lda CurrMap
            lsr                 ; shift lower bits of CurrMap into higher bits of MPL
            ror MapPtrL
            lsr
            ror MapPtrL
            clc
            adc #$10            ; map data starts at $1000.
            sta MapPtrH         ; floor(line/4) + $1000.

            ; the map pointer is to what is displayed at the TOP of the entire display
            ; the whole screen is 80 (28 30 28) long
            ; first mode 7 stuff is from 0 to 28.  Drawn from physical line $20.
            ; the mode 1 stuff is 28 lines down.  Drawn from physical line $09,
            ; and 6 lines long (magnified by 8 vertically)
            ; so its map data starts at 28+18-3 = 3D
            ; and goes to 42.
            ; then the next mode 7 stuff is from 58 to 80.  Drawn from line $78.

            lda #$20            ; top field starts at display line $20
            sta CurrLine        ; CurrLine is the current actual line on the screen
hiresline:  ldx CurrLine
            lda YHiresHA, x     ; get 2000-based address of current line on screen
            ; engineer it so that ZOtherZP in hgr pages always points to the other ZP to flip quickly.
            sta ZOtherZP        ; store HGR1 page in 1C00 ZP.
            sta R_ZP            ; go to HGR page 1 ZP
            pha                 ; stash it for putting in other ZP's ZOtherZP.
            clc
            adc #$20            ; second page is $2000 higher than first
            sta ZOtherZP        ; compute and store HGR page 2 ZP (in HGR1's ZP)
            sta R_ZP            ; go there (to HGR2 ZP)
            pla
            sta ZOtherZP        ; recall and store HGR page 1 ZP (in HGR2's ZP)
            lda #$1C            ; and go back to 1C00 ZP.
            sta R_ZP
            ; lo byte is same on either page, store it in 1C00 page.
            lda YHiresL, x
            sta ZLineStart
            ; push left edge right 14 pixels to center it
            inc ZLineStart
            inc ZLineStart
            ; point ZScrHole at the left side of present line in the map data.
            lda MapPtrL
            sta ZScrHole
            lda MapPtrH
            sta ZScrHole + 1
            lda #$81
            sta ZScrHole + XByte
            ; Zero, ZLineStart is now the left side of the draw line in graphics memory
            ; strategy: follow the map data line down the map. 
            ; draw top mode 7 part, middle mode 1 part, bottom mode 7 part.
            ; we have 64 map data bytes, will draw over 128 pixels.
            ; which really means drawing 63 bytes over 126 pixels.
            ; doubling, so using 4 bytes to represent 14 pixels and 7 map data bytes.
            ; mapbytes: 0  7  14  21  28  35  42  49  56  (63) (ZCurrMapX)
            ; pixbytes: 0  4   8  12  16  20  24  28  32  (36) (ZCurrDrawX)
            ; color lookup has color in both nibble to minimize shifting.
            ; diagram of layout borrowed frmo Rob Justice:
            ; 140x192 16 color mode pixel layout
            ; 
            ; |    2000     |    4000     |    2001     |    4001     |
            ; | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
            ; |  P1   |  P2   |  P3   |  P4   |  P5   |  P6   |  P7   |
            ;  B           B B           B B           B B           B
            ;  i           i i           i i           i i           i
            ;  t           t t           t t           t t           t
            ;  0           7 0           7 0           7 0           7
            lda #62
            sta ZCurrMapX        ; right edge of last group of map bytes
            lda ZLineStart       ; add 32 to left edge of graphics memory for line
            clc
            adc #32              ; to get left edge of last group of graphics memory for line
            sta ZCurrDrawX
            ; buffer the seven map elements we will represent in the stack.
            ; read them from right to left, then we draw them from left to right
toplineseg:
            ldy ZCurrMapX
            ldx #$06
:           lda (ZScrHole), y
            pha                 ; push buffered map elements onto the stack (safe from ZP switch)
            dey
            dex
            bpl :-
            sty ZCurrMapX       ; save new pointer for end of next (to the left) block after this
            ldx ZCurrDrawX      ; set x to the horizontal offset on screen
            lda ZOtherZP        ; go to HGR1 ZP for drawing
            sta R_ZP
            ; MapColors strategically have both upper and lower nibbles containing color
            ; byte 0 (page 1): -1110000 [0+0] 4218421
            pla                 ; first map element
            tay
            lda MapColors, y    ; color of pixels 0 and 1
            pha                 ; stash for later
            and #$7F
            sta Zero, x
            ; byte 0 (page 2): -3322221 [0+1+1] 2184218
            pla                 ; recall color of pixel 1
            bpl :+
            lda #$01
            bne :++
:           lda #$00
:           sta ZPxScratch      ; stash bit of pixel 1
            pla                 ; second map element
            tay
            lda MapColors, y    ; color of pixels 2 and 3
            pha                 ; stash for later
            asl
            and #%011111110
            ora ZPxScratch
            ; put this pixel data on the other ZP (page 2)
            ldy ZOtherZP
            sty R_ZP            ; go to page 2 ZP
            sta Zero, x
            ldy ZOtherZP
            sty R_ZP            ; go to page 1 ZP
            inx
            ; byte 1 (page 1): -5444433 [1+2+2] 1842184
            pla                 ; recall color of pixel 3
            lsr
            lsr
            and #$03            ; isolate the color's higher two bits
            sta ZPxScratch      ; and stash them
            pla                 ; third map element
            tay
            lda MapColors, y    ; color of pixels 4 and 5
            pha                 ; save for later
            asl
            asl
            ora ZPxScratch
            and #$7F
            sta Zero, x
            ; byte 1 (page 2): -6666555 [2+3] 8421842
            pla                 ; recall color of pixels 4 and 5
            lsr
            and #$07            ; chop off lowest bit
            sta ZPxScratch
            pla                 ; fourth map element
            tay
            lda MapColors, y    ; color of pixels 6 and 7
            pha                 ; stash for later
            lsr
            and #%01111000
            ora ZPxScratch
            ; put this data on the page 2 ZP
            ldy ZOtherZP
            sty R_ZP
            sta Zero, x
            ldy ZOtherZP
            sty R_ZP
            inx
            ; byte 2 (page 1): -8887777 [3+4] 4218421
            pla                 ; recall color of pixels 6 and 7
            and #$0F
            sta ZPxScratch
            pla                 ; fifth map element
            tay
            lda MapColors, y    ; color of pixels 8 and 9
            pha                 ; stash for later
            and #%01110000
            ora ZPxScratch
            sta Zero, x
            ; byte 2 (page 2): -AA99998 [4+4+5]  2184218
            pla                 ; recall color of pixels 8 and 9
            lsr
            lsr
            lsr
            sta ZPxScratch
            pla                 ; sixth map element
            tay
            lda MapColors, y    ; color of pixels A and B
            pha                 ; stash for later
            asl
            and #%01100000
            ora ZPxScratch
            ; put this data on the other ZP
            ldy ZOtherZP
            sty R_ZP            ; go to page 2 ZP
            sta Zero, x
            ldy ZOtherZP
            sty R_ZP            ; go to page 1 ZP
            inx
            ; byte 3 (page 1): -CBBBBAA [5+5+6] 1842184
            pla                 ; recall color of pixels A and B
            lsr
            lsr
            and #%00111111
            sta ZPxScratch
            pla                 ; seventh map element
            tay
            lda MapColors, y    ; colors of pixels C and D
            pha                 ; stash for later
            asl
            asl
            and #%01000000
            ora ZPxScratch
            sta Zero, x
            ; byte 3 (page 2): -DDDDCCC [6+6] 4218421
            pla                 ; recall color of pixels C and D
            lsr
            and #$7F
            ; put this data on the other ZP
            ldy ZOtherZP
            sty R_ZP            ; go to page 2 ZP
            sta Zero, x
            ldy #$1C
            sty R_ZP            ; go to 1C00 ZP

            ; the 14 pixels are now drawn
            ; continue back through the line
            ; the map pointer was already decremented when we buffered it.
            ; move the pointer for the (left edge of) drawn pixels back by 4 bytes.
            
            lda ZCurrDrawX
            sec
            sbc #$04
            sta ZCurrDrawX
            lda ZCurrMapX
            bmi :+          ; we had ran off the left edge of the line, so now we are done
            jmp toplineseg
            ; with the line now drawn, advance the map pointer to the next line
:           lda MapPtrL
            clc
            adc #$40
            sta MapPtrL
            bcc :+
            inc MapPtrH
            ; advance the graphics line
:           inc CurrLine
            lda CurrLine
            cmp #$A0            ; last line of bottom field complete? ($78-$A0)
            beq hiresdone       ; if so, move to the lower status section
            cmp #$48            ; last line of top field complete? ($20-$48)
            beq startmid        ; if so, move on to the middle part
            jmp hiresline       ; otherwise, do the next line
hiresdone:            
            jmp lowstats
startmid:
            ; top hires field complete.
            ; middle lores field starts at $09
            ; pointer should be pointing at 28, skip to 3D (+$15 lines = +540) and draw to $42.
            ; $100 is 4 lines ahead, $400 is $10 lines ahead, $500 is $14, $540 is $15.
            lda MapPtrL
            clc
            adc #$40
            sta MapPtrL
            lda MapPtrH
            adc #$05
            sta MapPtrH

            lda #$09
            sta CurrLine
loresline:
            lda MapPtrL
            sta ZScrHole
            lda MapPtrH
            sta ZScrHole + 1
            lda #$81
            sta ZScrHole + XByte
            ; (ZScrHole), 0 is now the left side of the map data line
            ; buffer all diplayed map bytes into the stack, from right to left
            ; NOTE: This needs to be updated once we can move lores view to the right.
            ldy #$27
:           lda (ZScrHole), y   ; load map data
            tax
            lda PlayChars, x    ; translate to displayed character
            pha                 ; store displayed character on stack
            lda PlayColors, x   ; translate to displayed color
            sta Zero, y         ; store color in 1C00 page
            dey
            bpl :-
            ; send to screen
            ldx CurrLine
            lda YLoresL, x
            sta ZLineStart
            lda YLoresHA, x     ; $400 base (character space)
            ldx ZLineStart
            sta R_ZP            ; go to character memory
            ; Zero, ZLineStart is now the left side of the draw line in graphics memory
            ; blast characters on stack onto character memory (left to right)
            ldy #$27
:           pla
            sta Zero, x
            inx
            dey
            bpl :-
            ; push the colors onto the stack so they are available from color page ZP
            lda #$1C
            sta R_ZP            ; go back to 1C00 ZP
            ldy #$27
:           lda Zero, y
            pha
            dey
            bpl :-
            ; draw the colors
            ldx CurrLine
            lda YLoresHB, x     ; $800 base (color space)
            ldx ZLineStart
            sta R_ZP            ; go to color space ZP
            ldy #$27
:           pla
            sta Zero, x
            inx
            dey
            bpl :-
            ; line now drawn, move to next one
            lda #$1C
            sta R_ZP            ; go back to 1C00 ZP
            ; advance map pointer
            lda MapPtrL
            clc
            adc #$40
            sta MapPtrL
            bcc :+
            inc MapPtrH
            ; advance screen line
:           inc CurrLine
            lda CurrLine
            cmp #$0F
            beq :+
            jmp loresline       ; more lines to draw, go draw them
:
            ; top and middle fields now drawn, go back and do the bottom one
            ; skip ahead $15 (from $43 to $58).
            lda MapPtrL
            clc
            adc #$40
            sta MapPtrL
            lda MapPtrH
            adc #$05
            sta MapPtrH

;            jmp updatedone
            ; switch to the lower hires field and draw it from line $78
            lda #$78
            sta CurrLine
            jmp hiresline
            
lowstats:
            ; draw the last field on the screen, the mode 1 lower status region
            ; TOOD

            ; restore ZP and come back
updatedone:
            lda ZPSave
            sta R_ZP

            ; restore bank to whatever it was
            
            lda BankSave
            sta R_BANK
            
            rts
            
Seed:		.byte	0

seedRandom:
			; grab a random number seed from the fastest part of the realtime clock.
			txa
			pha
			lda R_ZP       	; save the ZP register
			pha
			lda #$00
			sta R_ZP       	; request smallest RTC byte
			lda IO_CLOCK	; close enough to random for now
			sta Seed
			pla
			sta R_ZP		; restore zero page
			pla             ; restore X
			tax
			rts

; build the playfield representation
; the map is 64 units wide and 256 units tall
; so, that's $40 pages, I'll put it in $1000-$4FFF.
; and in bank 1 I guess.
; the random numbers aren't going to look very random
; unless I keep reseeding them, so I will.
; This is still deterministic, but at least it should look
; a little better.  This should be improved at some point.
; an individual location on the playfield can be 0 (empty)
; 1 (wall), 2 (another wall), 3 (a third wall).  Should
; be mostly empty space.

makefield:  lda #$81
            sta ZPtrA + XByte
            lda #$00
            sta ZPtrA
            lda #$10
            sta ZPtrA + 1
mfseed:     jsr seedRandom
            ldx Seed
            ldy #$3F
mfline:     lda Random, x
            lda ZPtrA       ; DEBUG- be less random
            and #$07
            sta (ZPtrA), y
            inx             ; next random number
            dey             ; next x coordinate
            bpl mfline
            lda ZPtrA       ; move to next map line
            clc
            adc #$40
            sta ZPtrA
            bcc mfseed
            inc ZPtrA + 1
            lda ZPtrA + 1
            cmp #$50
            bne mfseed
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
			sta ZPtrA + XByte
			sta ZPtrB + XByte
			ldx #$1F
 mapline:   lda YHiresL, x
			sta ZPtrA
			sta ZPtrB
			lda YHiresHA, x
			sta ZPtrA + 1
			lda YHiresHB, x
			sta ZPtrB + 1
			ldy #$27
			cpx #$10
			beq mapsolid
			cpx #$1F
			beq mapsolid
			lda #$5A
			ldy #$27
			sta (ZPtrA), y
			sta (ZPtrB), y
			dey
			lda #$00
:           sta (ZPtrA), y
			sta (ZPtrB), y
			dey
			bne :-
			lda #$5A
			sta (ZPtrA), y
			sta (ZPtrB), y
			bne :++
mapsolid:   lda #$7F
:           sta (ZPtrA), y
			sta (ZPtrB), y
			dey
			bpl :-
:           dex
            bpl mapline
            
            ; mode 7 A3 Hires
            ; lines 32-61(+8) and then lines 110-150(+8)
            ; there really isn't anything static here, so
            ; just clear it.
            ldx #32
clrhgr:     lda YHiresL, x
            sta ZPtrA
            sta ZPtrB
            lda YHiresHA, x
            sta ZPtrA + 1
            lda YHiresHB, x
            sta ZPtrB + 1
            lda #$00
            ldy #$27
:           sta (ZPtrA), y
            sta (ZPtrB), y
            dey
            bpl :-
            lda YHiresHC, x
            sta ZPtrA + 1
            lda YHiresHD, x
            sta ZPtrB + 1
            lda #$00
            ldy #$27
:           sta (ZPtrA), y
            sta (ZPtrB), y
            dey
            bpl :-
            inx
            cpx #159
            beq clrhgrdone
            cpx #70
            bne clrhgr
            ldx #110
            bne clrhgr
clrhgrdone: 
            rts

; put the pointers to line x into ZPtrA (char) and ZPtrB (color)

getlinex:
			lda YLoresL, x
			sta ZPtrA
			sta ZPtrB
			lda YLoresHA, x
			sta ZPtrA + 1
			lda YLoresHB, x
			sta ZPtrB + 1
			lda #$8F
			sta ZPtrA + XByte
			sta ZPtrB + XByte
			rts

; clear graphics pages (just fill with nonsense for now)

clearhgr:	lda #$00
			sta ZPtrA
			lda #$20
			sta ZPtrA + 1
			lda #$8F
			sta ZPtrA + XByte
			ldx #$80
			lda #$40
			ldy #$00
:			sta (ZPtrA), y
			iny
			bne :-
			inc ZPtrA + 1
			dex
			bne :-
			rts
			
; clear text page (blank page one, F0 colors on page two)

cleartext:  lda #$00
			sta ZPtrA
			lda #$04
			sta ZPtrA + 1
			lda #$8F
			sta ZPtrA + XByte
			ldx #$04
			lda #$A0
			ldy #$00
:           sta (ZPtrA), y
			iny
			bne :-
			inc ZPtrA + 1
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