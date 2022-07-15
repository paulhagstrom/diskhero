; DISKHERO definitions

; ZP use.  Convention: prefix addresses in ZP with Z so I can identify them
; in the code for cycle counting and addressing mode options.

XByte       = $1601     ; Interp extended address offset (for ZP 1A)
Zero1A      = $1A00     ; for those times you want to write to ZP without using ZP

; Used in interpreter (1A00) ZP.

Zero        = $00   ; steer clear of 00-27, used for buffering display

ZPtrA       = $28
ZPtrB       = $2A
ZPtrC       = $2C

; game state variables
ZDiskX      = $30   ; pointer to disk X coordinates data
ZDiskY      = $32   ; pointer to disk Y coordinates data
ZDiskType   = $34   ; pointer to disk type data
ZHoardX     = $36   ; pointer to hoarder X coodinate
ZHoardY     = $38   ; pointer to hoarder Y coordinate
ZHoardXX    = $3A   ; pointer to hoarder X coordinate of second segment
ZHoardYY    = $3C   ; pointer to hoarder Y coordinate of second segment
ZHoardXV    = $3E   ; pointer to hoarder X velocity
ZHoardYV    = $40   ; pointer to hoarder Y velocity
ZHoardSp    = $42   ; pointer to hoarder speed (delay ticks between moves)
ZHoardTick  = $44   ; pointer to hoarder current number of ticks left until movement
ZHoardAnim  = $46   ; pointer to hoarder current animation frame

ZMapTemp    = $4D   ; temporary storage for a map byte being tested for collision
ZXXTemp     = $4E   ; temporary storage for second segment x-coordinate
ZFrame      = $4F   ; temporary storage for current animation frame

ZBorderV    = $50   ; playfield border counter (total vertical border columns)
ZBorderR    = $51   ; playfield border counter (total border columns on the right)
ZBorderRYet = $52   ; playfield border counter (right border columns yet to draw)

PTopRastA   = $6B   ; hires map parm: first copy target raster line upper field
PTopRastD   = $6C   ; hires map parm: raster line for drawing new upper field line
PBotRastA   = $6D   ; hires map parm: first copy target raster line lower field
PBotRastD   = $6E   ; hires map parm: raster line for drawing new lower field line
PTopMapOff  = $6F   ; hires map parm: map offset back from HeroY for new line

PNudge      = $70
TouchedVoid = $71
MapOffset   = $72
PInc        = $73

; lookup tables
ZFontDots   = $80   ; ZP cache for FontDots to speed up drawing (80-A7)
ZFontCol    = $A8   ; ZP cache for FontCol to speed up drawing (A8-D0)

; interrupt handler
ZIntBank    = $D0   ; place to stash the bank register value
ZPlaySound  = $D1   ; set to zero to silence music
ZPlaySFX    = $D2   ; set to zero to silence both sfx and music
ZSoundPtr   = $D8   ; current sample in background music
ZFXPtr      = $DA   ; current sample in sound effect
ZFXPlay     = $DC   ; nonzero if a sound effect is playing

ZBackNext   = $DF   ; page number of first sample of next background segment after this one

; Used when ZP is pointed at a display buffer
; these are not in conflict with addresses in the 1A00 ZP,
; generally limited to screen holes: 78-7F, F8-FF

; playfield
ZThumbTogg  = $78
ZThumbNext  = $79

ZScrHole    = $78
ZOtherZP    = $7A
ZCurrDrawX  = $7B
ZLineStart  = $7C
ZNumPtr     = $7D   ; pointer for screen target for drawnumber
ZBufCount   = $7F   ; count for buffering map data
ZCurrMapX   = $7E
ZMapBuffer  = $F8
ZPxScratch  = $FF

; from gamefont (conflicts not super important)

ZDataHole   = $F0
ZAsciiHole  = $F2
ZFontPtr    = $F4
ZCtlTemp    = $F6

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
RE_T1CL     = $FFE6     ; ffex timer1 count low byte
RE_T1CH     = $FFE7     ; ffex timer1 count high byte
RE_T2CL     = $FFE8     ; ffex timer2 count low byte
RE_T2CH     = $FFE9     ; ffex timer2 count high byte
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
