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
TERMINATE   = $65
;REQUEST_SEG = $40
;RELEASE_SEG = $45
;OPEN        = $C8
;READ        = $CA
;CLOSE       = $CC

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

ExitFlag:   .byte   0               ; keyboard int makes this nonzero to trigger exit
RedrawMap:  .byte   0               ; keyboard int makes this nonzero to trigger redraw
RedrawPlay: .byte   0               ; keyboard int makes this nonzero to trigger redraw
CurrTop:    .byte   0
CurrBottom: .byte   0
CurrPlay:   .byte   0

GameLevel:  .byte   0
GameScore:  .byte   0, 0, 0
ScrRegion:  .byte   0
FieldH:     .byte   $04, $05, $05, $06, $06, $07
FieldL:     .byte   $A8, $25, $A8, $28, $A8, $28
FieldHC:    .byte   $08, $09, $09, $0A, $0A, $0B
MapColors:  .byte   $00, $CC, $DD, $EE, $44
PlayColors: .byte   $00, $1C, $2D, $3E, $C4
PlayChars:  .byte   $41, $C2, $43, $21, $C7
; Screen layout:
; mode 1 (text)     lines 00-0F (10) 00-01  score
; mode 6 (bw hires) lines 10-1F (10)        b/w map display
; mode 7 (a3 hires) lines 20-47 (28)        hires map upper field  map: 00-27 
; mode 1 (text)     lines 48-7F (38) 09-0F  text play field        map: 28-5F show: 42-46 (5)
; mode 7 (a3 hires) lines 80-A7 (28)        hires map lower field  map: 60-87
; mode 1 (text)     lines A8-BF (18) 15-17  text status display
; Define the screen region; mode is a display mode, length is number of HBLs.
; nudge is 0 if no nudge, else pos or neg depending on which nudge count to use
ScrRegLen:  .byte   $0E, $0E, $27, $37, $27, $17, $00
ScrRegMode: .byte   $01, $06, $07, $01, $07, $01, $00
ScrNudge:   .byte   $00, $80, $01, $00, $01, $00, $00
NudgePos:   .byte   0
NudgeNeg:   .byte   0

; I played with ScRegLen by trial and error a little.
; Not sure why I needed to go two down for region zero.
; Probably because the VBL code (or HBL code) takes long enough that we miss
; some HBLs here and there.

ZFontDots   = $80   ; ZP cache for FontDots to speed up drawing
ZBufCount   = $81   ; count for buffering map data

; these are in screen holes because we're using screen memory for ZP
ZScrHole    = $78
ZOtherZP    = $7A
ZCurrDrawX  = $7B
ZLineStart  = $7C
ZCurrMapX   = $7E
ZMapBuffer  = $F8
ZPxScratch  = $FF
Zero        = $00
CurScrLine: .byte   0
CurMapLine: .byte   0
MapPtrL:    .byte   0
MapPtrH:    .byte   0
StashZP:    .byte   0

; we need to prioritize interrupts a bit because they can pile up.
; if HBL has priority we can wind up in an situation where nothing
; else gets a chance to fire.  So keyboard first, then VBL, then HBL/timer.

inthandle:  pha                 ; save registers
            tya
            pha
            txa
            pha
            cld
            lda RE_INTFLAG      ; identify interrupt
            and #$10            ; VBL?
            beq :+              ; nope
            jsr intvbl          ; handle the VBL interrupt
            lda #$10            ; clear CB1 VBL
            sta RE_INTFLAG
            bne intreturn
:           lda RE_INTFLAG
            and #$01            ; keyboard?
            beq :+              ; nope
            jsr intkey          ; handle the keyboard interrupt
            lda #$01            ; clear CA2 keyboard
            sta RE_INTFLAG
            bne intreturn
:           lda RE_INTFLAG
            and #$20            ; timer/HBL?
            bne inttimer        ; yep, go handle the timer/HBL
intreturn:  pla                 ; restore registers
            tax
            pla
            tay
            pla
            rti
; timer2 (HBL) interrupt handler
; goes off only at display mode switch points (not every HBL)
; sets the display mode and smooth scroll offset, then timer for next mode switch point
; allows for two different smooth scroll offsets, named "NudgePos" and "NudgeNeg"
inttimer:   inc ScrRegion       ; advance region
            ldx ScrRegion
            lda ScrRegMode, x   ; move display to correct mode
            jsr setdisplay
            lda D_SCROLLOFF     ; nudge off
            sec                 ; set up bcs/jmp
            lda ScrNudge, x     ; nudge if this region needs nudging
            beq postnudge       ; do no nudging
            bmi negnudge        ; using negative (alt) nudge?
            lda NudgePos        ; nope, use the positive (regular) nudge value
            bcs gonudge
negnudge:   lda NudgeNeg        ; yep, use the negative (alternate) nudge value
gonudge:    jsr setnudge        ; twiddle the nudge bits
            lda D_SCROLLON      ; turn on nudging
postnudge:  lda ScrRegLen, x    ; reset the HBL counter to length of next mode
            sta RE_T2CL
            lda #$00            ; clear the timer2 flag
            sta RE_T2CH
            lda #$20
            sta RE_INTFLAG
            bne intreturn
; keyboard interrupt handler
intkey:     lda IO_KEY          ; load keyboard register
            sta IO_KEYCLEAR     ; clear keyboard register
            bpl keyreturn       ; no key pressed, return
            sta $0400           ; put it in the corner so I can see it
            cmp #$C9            ; I (up)
            bne :+
            inc RedrawPlay
            dec NudgePos
            bne keyreturn
            lda #$07
            sta NudgePos
            dec CurrMap
            inc RedrawMap
            bne keyreturn
:           cmp #$CD            ; M (down)
            bne :+
            inc RedrawPlay
            inc NudgePos
            lda NudgePos
            cmp #$08
            bne keyreturn
            lda #$00
            sta NudgePos
            inc CurrMap
            inc RedrawMap
            bne keyreturn
:           cmp #$CA            ; J (left)
            bne :+
            inc RedrawPlay
:           cmp #$CB            ; K (right)
            bne :+
            inc RedrawPlay
:           cmp #$C5            ; E (exit)
            bne keyreturn
            inc ExitFlag        ; tell event loop we are exiting
keyreturn:  rts

; VBL interrupt handler
intvbl:     lda ScrRegLen       ; reset the HBL counter to the length of top region
            sta RE_T2CL
            lda #0
            sta RE_T2CH
            sta ScrRegion       ; reset region number
            lda ScrRegMode      ; move display to correct mode
            jsr setdisplay
            lda D_SCROLLOFF     ; smooth scrolling off is assumed for top region
            rts

init:       sei                 ; no interrupts while we are setting up
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
            lda #$1A            ; ZP to $1A00 (standard for interpreters)
            sta R_ZP
            jsr herofont        ; load game font into character RAM
            ldx #$27            ; pull the FontDots info into ZP for faster access
:           lda FontDots, x
            sta ZFontDots, x
            dex
            bpl :-
            jsr drawback        ; draw static background
            jsr makefield       ; set up map
            jsr setupenv        ; arm interrupts
            lda #$00
            sta ScrRegion
            lda #$80            ; start at line 80 of the map, bottom half
            sta CurrMap
            lda #$00            ; start at nudge 0
            sta NudgePos
            sta NudgeNeg
            lda #$00
            sta GameLevel
            lda #$00
            sta GameScore
            lda #$01
            sta RedrawMap       ; start by assuming we need to redraw whole thing
            bit IO_KEYCLEAR
            cli                 ; all set up now, interrupt away
eventloop:  lda ExitFlag
            bne alldone
            jsr drawscore
            inc GameScore + 2   ; DEV - constantly move score up
            lda RedrawPlay      ; screen moved, at least playfield needs update
            bne :+
            jsr scrupdate
:           jmp eventloop

alldone:    lda #$7F            ;disable all interrupts
            sta RD_INTENAB
            sta RD_INTFLAG
            lda #$7F
            sta RE_INTENAB
            sta RE_INTFLAG

            brk                  ; SOS TERMINATE
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
            lda #$4C             ; jmp
            sta IRQVECT
            lda #<inthandle
            sta IRQVECT + 1
            lda #>inthandle
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

mapattern:
            ; pattern 0
            .byte   %00100010
            .byte   %00110100
            ; pattern 1
            .byte   %01100010
            .byte   %00110110
            ; pattern 2
            .byte   %10100110
            .byte   %00110100
            ; pattern 3
            .byte   %00101010
            .byte   %10110101

; compute map pointer, based on A.  Map data is in bank 2, $1000-4FFF.
; If CurrMap is something like 00000101 (5), shift bits to translate to:
; MapPtrL: 01000000 (40) MapPtrH: 00010001 (11) ($1140 and $40 bytes there)

getmapptr:  pha
            lda #$00
            sta MapPtrL
            pla
            lsr                 ; shift lower bits of map line into higher bits of MPL
            ror MapPtrL
            lsr
            ror MapPtrL
            ora #$10            ; map data starts at $1000.
            sta MapPtrH         ; floor(line/4) + $1000.
            rts

; screen layout:
; 
; the top hires portion of the screen occupies "lines $20 to $48."
; on the screen, due to nudging, we draw from lines $20 to $4F.
; the map repsents 2x2 patterns, so we draw map lines from N to N+$20
; the middle text section represents $28 across, drawn from line $09 to line $0E.
; it obscures $18 map lines, displaying the middle 6.  So it represents N+$26 to N+$2C.
; then the lower section picks up from N+$38 to N+$58.
; the text section is always redrawn and is not nudged. Its top map line is N+$26+nudge.
; YOU ARE HERE
; ACTUALLY, I think I may want to nudge the text section sort of.
; have text characters that represent on and off zero?
; BUT: if something is off zero, though, would we expect something to be able to be up close to it?
; like if a wall block stops halfway up, shouldn't a character be able to be up next to it?
; so maybe would be better if playfield were GIANT?
; so that it is just three map lines, double tall?  The we would not need partial characters.
; But, then it also doesn't make the nice point. So maybe I just keep people from getting too close
; to walls.

; characters 01-0F (walls and disk) have color information
; MapColors provides a mapping between 2-bit color and displayed color
; in the hires map, we will use that color for those elements

; copied from the top just for ease of locating it
; Screen layout:
; mode 1 (text)     lines 00-0F (10) 00-01  score
; mode 6 (bw hires) lines 10-1F (10)        b/w map display
; mode 7 (a3 hires) lines 20-47 (28)        hires map upper field  map: 00-27 
; mode 1 (text)     lines 48-7F (38) 09-0F  text play field        map: 28-5F show: 42-46 (5)
; mode 7 (a3 hires) lines 80-A7 (28)        hires map lower field  map: 60-87
; mode 1 (text)     lines A8-BF (18) 15-17  text status display

scrupdate:
            ; update parts of the screen that need updating.
            ; redrawPlay is set if the playfield in the middle needs updating
            ; redrawMap is set if the map above and below playfield needs updating too
            ; The map can be moved by NudgePos without redrawing, while the playfield cannot.
            ; CurrTop is the top map line of the top map we are looking at.
            ; CurrBottom is the top map line of the bottom map we are looking at.
            ; CurrPlay is the top map line of the playfield.
            ; Those three should always be kept in sync programmatically, but no need to
            ; burn cycles continually adding things together when we could just cache.
            ; The map lines will represent patterns that are 2 by 4, which roughly matches the
            ; proportions of the 40 column characters.
            ; 
            ; update playfield (lines 9-14)
            ; CurrMap represents the line in the map data at top of playfield
            ; playfield representation starts at 2000 in bank 2, each line is $40 long
            ; so lines at 2000, 2040, 2080, 20C0, 2100, 2140, etc.
            ; anything interacting with the map should activate $1A00 ZP
            ; we will temporarily switch to graphics-based ZP when necessary
            lda R_ZP
            sta ZPSave          ; save current ZP
            lda R_BANK
            sta BankSave        ; save current bank
            lda #$1A
            sta R_ZP            ; switch ZP to $1A00
            lda #$00            ; swap in bank zero,
            sta R_BANK          ; where (hires) graphics memory lives

            lda RedrawMap
            bne redrawmap
            ; we only need to redraw the play field, so start the map pointer at $42
            lda #$42
            jsr getmapptr
            jmp redrawplay            
redrawmap:
            lda #$20            ; top field starts at display line $20
            sta CurScrLine      ; CurScrLine is the current actual line on the screen
            lda CurrMap
            jsr getmapptr       ; load mapptr for CurrMap
hiresline:
            ; prepare ground for pushing to this raster line
            ldx CurScrLine
            lda YHiresHA, x     ; get 2000-based address of current line on screen
            ; engineer it so that ZOtherZP in hgr pages always points to the other ZP to flip quickly.
            sta ZOtherZP        ; store HGR1 page in 1A00 ZP.
            sta R_ZP            ; go to HGR page 1 ZP
            pha                 ; stash it for putting in other ZP's ZOtherZP.
            clc
            adc #$20            ; second page is $2000 higher than first
            sta ZOtherZP        ; compute and store HGR page 2 ZP (in HGR1's ZP)
            sta R_ZP            ; go there (to HGR2 ZP)
            pla
            sta ZOtherZP        ; recall and store HGR page 1 ZP (in HGR2's ZP)
            lda #$1A            ; and go back to 1A00 ZP.
            sta R_ZP
            ; lo byte is same on either page, store it in 1A00 page.
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
            lda #$82
            sta ZScrHole + XByte
            ; we have 64 map data bytes, will draw over 128 pixels.
            ; which really means drawing 63 bytes over 126 pixels.
            ; using 4 bytes to represent 14 pixels and 7 map data bytes.
            ; mapbytes: 0  7  14  21  28  35  42  49  56  (63) (ZCurrMapX)
            ; pixbytes: 0  4   8  12  16  20  24  28  32  (36) (ZCurrDrawX)
            ; color lookup has color in both nibble to minimize shifting.
            ; diagram of pixel/video memory layout borrowed frmo Rob Justice:
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
            lda #$06            ; we will buffer seven map elements
            sta ZBufCount
            ldy ZCurrMapX
bufmap:     lda (ZScrHole), y
            ; now that we have the byte from the map, we can tranlate this into
            ; the two pixels it will be displaying.
            ; this information comes from FontDots, which we cached into ZFontDots (1A ZP)
            bit #$70            ; test to see if this is 0-F (separate color info)
            beq :+              ; branch if this is an element with an indexed color
            tax                 ; this is an element with an intrinsic color (in ZFontDots)
            lda ZFontDots, x
            sec
            bcs bufmap
:           pha                 ; stash the pixel data
            and #$C0            ; isolate the color
            clc
            rol
            rol
            rol                 ; convert bits 6/7 to bits 0/1 (color 0-3)
            tax
            lda MapColors, x    ; load the indexed color
            sta ZPxScratch
            pla                 ; get the pixel data back
            and ZPxScratch      ; apply the color
bufmappix:  pha                 ; push buffered map elements onto the stack (safe from ZP switch)
            dey
            dec ZBufCount
            bpl bufmap
            sty ZCurrMapX       ; save new pointer for end of next (to the left) block after this
            ; the pixels have now been translated, we can send them to the screen
            ; the 7 pixels on the stack each use 8 bits, but we need to smear them across the
            ; 8 bytes of graphics memory using 7 bits at a time.  I know, right?
            ldx ZCurrDrawX      ; set x to the horizontal offset on screen
            lda ZOtherZP        ; go to HGR1 ZP for drawing
            sta R_ZP
            ; byte 0 (page 1): -1110000 [0+0] 4218421
            pla                 ; pixels 0-1
            pha                 ; remember for later
            and #$7F
            sta Zero, x
            ; byte 0 (page 2): -3322221 [0+1+1] 2184218
            pla                 ; recall color of pixel 1
            bpl :+
            lda #$01
            bne :++
:           lda #$00
:           sta ZPxScratch      ; stash bit of pixel 1
            pla                 ; pixels 2-3
            pha                 ; remember for later
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
            and #$03            ; isolate the pixel 3 color's higher two bits
            sta ZPxScratch      ; and stash them
            pla                 ; pixels 4-5
            pha                 ; remember for later
            asl
            asl
            ora ZPxScratch
            and #$7F
            sta Zero, x
            ; byte 1 (page 2): -6666555 [2+3] 8421842
            pla                 ; recall color of pixel 5
            lsr
            and #$07            ; chop off lowest bit
            sta ZPxScratch
            pla                 ; pixels 6-7
            pha                 ; remember for later
            lsr
            and #%01111000
            ora ZPxScratch
            ; put this data on the page 2 ZP
            ldy ZOtherZP
            sty R_ZP            ; go to page 2 ZP
            sta Zero, x
            ldy ZOtherZP
            sty R_ZP            ; go to page 1 ZP
            inx
            ; byte 2 (page 1): -8887777 [3+4] 4218421
            pla                 ; recall color of pixel 7
            and #$0F
            sta ZPxScratch
            pla                 ; pixels 8-9
            pha                 ; remember for later
            and #%01110000
            ora ZPxScratch
            sta Zero, x
            ; byte 2 (page 2): -AA99998 [4+4+5]  2184218
            pla                 ; recall color of pixels 8 and 9
            lsr
            lsr
            lsr
            sta ZPxScratch
            pla                 ; pixels A-B
            pha                 ; remember for later
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
            pla                 ; recall color of pixel A
            lsr
            lsr
            and #%00111111
            sta ZPxScratch
            pla                 ; pixels C-D
            pha                 ; remember for later
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
            ldy #$1A
            sty R_ZP            ; go to 1A00 ZP

            ; the 14 pixels are now drawn
            ; continue back through the line
            ; the map pointer was left pointing in the right place after we buffered it.
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
            ; advance the graphics raster line
:           inc CurScrLine
            lda CurScrLine
            cmp #$A8            ; last line of bottom field complete? ($80-$A7)
            beq hiresdone       ; if so, move to the next screen region
            cmp #$48            ; last line of top field complete? ($20-$47)
            beq startmid        ; if so, move on to the middle playfield region
            jmp hiresline       ; otherwise, do the next line
hiresdone:            
            jmp lowstats
startmid:
            ; the middle lores field starts at map offset $42 and draws to offset $46
            ; after drawing top hires, map pointer will be pointing at $28.
            ; so advance it by $1A map lines (x $40 = $0680 bytes)
            ; $100 is 4 lines ahead, $400 is $10 lines ahead, $500 is $14, $600 is $18, $680 is $1A.
            ; TODO: Account for nudging of the hires field, needs to offsel lores field.
            lda MapPtrL
            clc
            adc #$80
            sta MapPtrL
            lda MapPtrH
            adc #$06
            sta MapPtrH
redrawplay:
            lda #$09
            sta CurScrLine
loresline:
            lda MapPtrL
            sta ZScrHole
            lda MapPtrH
            sta ZScrHole + 1
            lda #$82
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
            sta Zero, y         ; store color in 1A00 page
            dey
            bpl :-
            ; send to screen
            ldx CurScrLine
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
            lda #$1A
            sta R_ZP            ; go back to 1A00 ZP
            ldy #$27
:           lda Zero, y
            pha
            dey
            bpl :-
            ; draw the colors
            ldx CurScrLine
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
            lda #$1A
            sta R_ZP            ; go back to 1A00 ZP
            ; advance map pointer
            lda MapPtrL
            clc
            adc #$40
            sta MapPtrL
            bcc :+
            inc MapPtrH
            ; advance screen line
:           inc CurScrLine
            lda CurScrLine
            cmp #$0F
            beq :+
            jmp loresline       ; more lines to draw, go draw them
:
            ; if we only needed to redraw the playfield we are done
            lda RedrawMap
            bne :+
            jmp hiresdone
            
            ; top and middle fields now drawn, go back and do the bottom one
            ; skip ahead $15 (from $43 to $58).
:           lda MapPtrL
            clc
            adc #$40
            sta MapPtrL
            lda MapPtrH
            adc #$05
            sta MapPtrH

;            jmp updatedone
            ; switch to the lower hires field and draw it from line $78
            lda #$78
            sta CurScrLine
            jmp hiresline
            
lowstats:
            ; draw the last field on the screen, the mode 1 lower status region
            ; TOOD

            ; restore ZP and come back
updatedone:
            ; reset the update flags
            lda #$00
            sta RedrawMap
            sta RedrawPlay
            
            lda ZPSave
            sta R_ZP

            ; restore bank to whatever it was
            
            lda BankSave
            sta R_BANK
            
            rts
            
Seed:        .byte    0

seedRandom:
            ; grab a random number seed from the fastest part of the realtime clock.
            lda #$00
            sta R_ZP           ; request smallest RTC byte
            lda IO_CLOCK    ; close enough to random for now
            sta Seed
            lda #$1A
            sta R_ZP
            rts

; build the map.
; the map is $40 units wide and $100 units tall.  Lives in bank 2, $1000-4FFF.
; map bytes have shape info in lower 6 bits, color/variant info in higher 2

; 6x6 box patterns for adding to the map.  Each pattern is $24 bytes long.
BoxIndex:   .byte 0, $24, $48, $6C
BoxPatt:
            ; box open leftward
            .byte C_WALL_R, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_LD
            .byte C_SPACE, C_SPACE, C_SPACE, C_SPACE, C_SPACE, C_WALL_V
            .byte C_SPACE, C_SPACE, C_SPACE, C_WALL_R, C_WALL_H, C_WALL_LUD
            .byte C_SPACE, C_SPACE, C_SPACE, C_SPACE, C_SPACE, C_WALL_V
            .byte C_SPACE, C_SPACE, C_SPACE, C_SPACE, C_SPACE, C_WALL_V
            .byte C_WALL_R, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_LU
            ; box open rightward
            .byte C_WALL_RD, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_L
            .byte C_WALL_V, C_SPACE, C_SPACE, C_SPACE, C_SPACE, C_SPACE
            .byte C_WALL_RUD, C_WALL_H, C_WALL_L, C_SPACE, C_SPACE, C_SPACE
            .byte C_WALL_V, C_SPACE, C_SPACE, C_SPACE, C_SPACE, C_SPACE
            .byte C_WALL_V, C_SPACE, C_SPACE, C_SPACE, C_SPACE, C_SPACE
            .byte C_WALL_RU, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_L
            ; box open upward
            .byte C_WALL_D, C_SPACE, C_SPACE, C_SPACE, C_SPACE, C_WALL_D
            .byte C_WALL_V, C_SPACE, C_SPACE, C_SPACE, C_SPACE, C_WALL_V
            .byte C_WALL_V, C_SPACE, C_SPACE, C_SPACE, C_SPACE, C_WALL_V
            .byte C_WALL_V, C_SPACE, C_WALL_D, C_SPACE, C_SPACE, C_WALL_V
            .byte C_WALL_V, C_SPACE, C_WALL_V, C_SPACE, C_SPACE, C_WALL_V
            .byte C_WALL_RU, C_WALL_H, C_WALL_LRU, C_WALL_H, C_WALL_H, C_WALL_LU
            ; box open downward
            .byte C_WALL_RD, C_WALL_H, C_WALL_LRD, C_WALL_H, C_WALL_H, C_WALL_LD
            .byte C_WALL_V, C_SPACE, C_WALL_V, C_SPACE, C_SPACE, C_WALL_V
            .byte C_WALL_V, C_SPACE, C_WALL_U, C_SPACE, C_SPACE, C_WALL_V
            .byte C_WALL_V, C_SPACE, C_SPACE, C_SPACE, C_SPACE, C_WALL_V
            .byte C_WALL_V, C_SPACE, C_SPACE, C_SPACE, C_SPACE, C_WALL_V
            .byte C_WALL_U, C_SPACE, C_SPACE, C_SPACE, C_SPACE, C_WALL_U
            
MFX         .byte 0
MFY         .byte 0
MFColor     .byte 0
MFBoxIndex  .byte 0
MFPlaced    .byte 0
    
makefield:  lda R_ZP
            sta ZPSave
            lda #$1A        ; go to 1A00 ZP (should already be there)
            sta R_ZP
            lda #$00
            sta ZPtrA
            lda #$10
            sta ZPtrA + 1
            lda #$82
            sta ZPtrA + XByte
            ; seed the "random" number list
            jsr seedRandom
            ; zero out the map background
            ldx #$40        ; clearing $40 pages
            lda #$00
            tay
mfzero:     sta (ZPtrA), y
            iny
            bne mfzero
            inc ZPtrA + 1
            dex
            bne mfzero
            ; place some boxes
            ; every 8x8 cell gets a 6x6 box centered in it
            ; since we're always going one down and one in, start at 1, 1
            lda #$01
            sta MFX
            lda #$01
            sta MFY
            ldx Seed
            lda Random, x   ; pick a random box shape
            and #$03
            tay
            lda BoxIndex, y
            sta MFBoxIndex
            inx
            lda Random, x   ; pick a random box color
            and #$C0
            sta MFColor
            inx
            stx Seed
            ; locate address in map for coordinate MFX, MFY
            lda #$00
            sta ZPtrA
            lda MFY
            lsr
            ror ZPtrA
            lsr
            ror ZPtrA
            clc
            adc #$10
            sta ZPtrA + 1
            lda ZPtrA
            clc
            adc MFX
            sta ZPtrA
            ; put a pattern row in the map
            ldy #$05
            sty MFPlaced        ; do 6 rows
mfpattrow:  ldx MFBoxIndex
:           lda BoxPatt, x
            ora MFColor
            sta (ZptrA), y
            dex
            dey
            bpl :-
            ; done if we have completed 6 rows now
            dec MFPlaced
            bmi :+
            ; move to next row
            lda MFBoxIndex
            clc
            adc #$06
            sta MFBoxIndex
            lda ZPtrA
            clc
            adc #$40
            sta ZPtrA
            bcc mfpattrow
            inc ZPtrA + 1
            bcs mfpattrow
:
; TODO place some disks
; TODO place some hoarders
            lda ZPSave
            sta R_ZP
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

; initialize graphics and draw static background
drawback:   lda #$01            ; Apple III color text
            jsr setdisplay
            bit D_PAGEONE
            bit D_SCROLLOFF
            lda #$00
            jsr setnudge
            jsr cleartext
            jsr clearhgr
            ; text lines 00-01: score status
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
            ; text lines 15-17: progress status
            ldy #$27
:           lda StatTextA, y
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
            ; text lines 09-0F: playfield (frame)
            ldy #$27
:           lda FrameText, y
            sta $4A8, y
            sta $7A8, y
            lda FrameCol, y
            sta $8A8, y ; 
            sta $BA8, y ; 
            lda InnerText, y
            sta $528, y
            sta $5A8, y
            sta $628, y
            sta $6A8, y
            sta $728, y
            lda InnerCol, y
            sta $928, y
            sta $9A8, y
            sta $A28, y
            sta $AA8, y
            sta $B28, y
            dey
            bpl :-
            ; mode 6 super hires
            ; 16 lines, from 10-1F.
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
            ; lines 20-47(+8) and then lines 80-A7(+8)
            ; there really isn't anything static here, so
            ; just clear it.
            ldx #$20
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
            cpx #$AF            ; one past bottom field + 8
            beq clrhgrdone
            cpx #$50            ; one past top field + 8
            bne clrhgr
            ldx #$80            ; skip to start of bottom field
            bne clrhgr
clrhgrdone: 
            rts

; clear graphics pages (just fill with nonsense for now)

clearhgr:   lda #$00
            sta ZPtrA
            lda #$20
            sta ZPtrA + 1
            lda #$8F
            sta ZPtrA + XByte
            ldx #$80
            lda #$40
            ldy #$00
:           sta (ZPtrA), y
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
            
            .include "gamefont.s"

CodeEnd     = *