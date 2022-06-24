; split
; Paul Hagstrom, 2022
; use HBL and VBL interrupts to split graphics modes

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
			; reset the HBL counter
			lda #24        ; for now reset count to 24 lines
			sta RE_T2CL
			lda #$00
			sta RE_T2CH
			; switch to the non-current mode
			lda CurrMode
			eor #%00001110	; switch between modes 1 and 7
			sta CurrMode
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
			; reset the HBL counter
			lda #24
			sta RE_T2CL
			lda #0
			sta RE_T2CH
			; go back to mode 1
			lda #$01
			sta CurrMode
			jsr setdisplay
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
			lda #$01            ; Apple III color text
			sta CurrMode
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

CountVBL:   .byte	0, 0, 0, 7
CountKBD:   .byte	0, 0, 0, 7
CountHBL:   .byte	0, 0, 0, 7

TextOne:    .byte	"MODE SPLITTING", 0
CurrColor:	.byte	0
CurrLine:	.byte	0

; paint the static parts of the page

paintback:  
			lda #23        ; vtab
			sta CurrLine
			lda #$E0        ; vtab
			sta CurrColor
:			ldx CurrLine
			jsr getlinex
			lda #>TextOne
			pha
			lda #<TextOne
			pha
			lda CurrColor
			pha
			jsr printstr
			lda CurrLine
			and #$0F
			ora #$E0
			sta CurrColor
			dec CurrLine
			bpl :-
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

; clear graphics pagges (just fill with nonsense for now)

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
						
CodeEnd     = *