; DISKHERO definitions
;


; indirect addressing
; trying to collect my ZP use in here so that I avoid collisions.
; by convention also trying to prefix ZP addresses with Z so that
; I know when they might not survive a shift in ZP.

XByte       = $1601     ; Interp extended address offset
ZPtrA       = $20
ZPtrB       = $22

ZDiskType   = $24
ZDiskX      = $26
ZDiskY      = $28
ZHoardX     = $2A
ZHoardY     = $2C
ZHoardXV    = $2E
ZHoardYV    = $30

ZFontDots   = $80   ; ZP cache for FontDots to speed up drawing
ZFontCol    = $40   ; ZP cache for FontCol to speed up drawing
ZBufCount   = $7F   ; count for buffering map data
ZNumPtr     = $7D   ; pointer for screen target for drawnumber

; these are in screen holes because we're using screen memory for ZP
ZScrHole    = $78
ZOtherZP    = $7A
ZCurrDrawX  = $7B
ZLineStart  = $7C
ZCurrMapX   = $7E
ZMapBuffer  = $F8
ZPxScratch  = $FF
Zero        = $00
Zero1A      = $1A00

PTopRastA   = $6B
PTopRastD   = $6C
PBotRastA   = $6D
PBotRastD   = $6E
PTopMapOff  = $6F

PNudge      = $70
TouchedVoid = $71
MapOffset   = $72
PInc        = $73

; from gamefont

ZDataHole   = $A0
ZAsciiHole  = $A2
ZFontPtr    = $A4
ZCtlTemp    = $A6


; I/O softswitches

IO_KEY      = $C000     ; keyboard
IO_KEYFLAG  = $C008     ; keyboard flag
IO_KEYCLEAR = $C010     ; keyboard strobe

; graphics switches

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

D_SCROLLOFF = $C0D8     ; disable smooth scroll
D_SCROLLON  = $C0D9     ; enable smooth scroll (vertical shift)

F_XFERON    = $C0DB     ; turn on font transfer
F_XFEROFF   = $C0DA     ; turn off font transfer

; VIA registers

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

; SOS Calls

TERMINATE   = $65
;REQUEST_SEG = $40
;RELEASE_SEG = $45
;OPEN        = $C8
;READ        = $CA
;CLOSE       = $CC
