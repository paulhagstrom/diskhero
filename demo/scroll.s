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

; we have from A000 to B800 before SOS arrives

            .org     $A000 - 14
            
; sos interp header
            .byte    "SOS NTRP"
            .word    0000
            .word    CodeStart
            .word    (CodeEnd-CodeStart)

CodeStart:  jmp init

            .include "lookups.s"
            .include "gamefont.s"

IRQSave:    .byte   0, 0 , 0        ; saved state
ZPSave:     .byte   0
BankSave:   .byte   0

ExitFlag:   .byte   0               ; keyboard int makes this nonzero to trigger exit
KeyCaught:  .byte   0               ; keyboard int pushes a caught key in here
RedrawMap:  .byte   0               ; keyboard int makes this nonzero to trigger redraw
RedrawPlay: .byte   0               ; keyboard int makes this nonzero to trigger redraw
CurrMap:    .byte   0

GameLevel:  .byte   0
GameScore:  .byte   0, 0, 0
ScrRegion:  .byte   0
FieldH:     .byte   $04, $05, $05, $06, $06, $07
FieldL:     .byte   $A8, $25, $A8, $28, $A8, $28
FieldHC:    .byte   $08, $09, $09, $0A, $0A, $0B
MapColors:  .byte   $88, $66, $33, $44
PlayColors: .byte   $08, $06, $03, $04
; Screen layout:
; mode 1 (text)     lines 00-0F (10) 00-01  score
; mode 6 (bw hires) lines 10-1F (10)        b/w map display
; mode 7 (a3 hires) lines 20-47 (28)        hires map upper field  map: 00-27 
; mode 1 (text)     lines 48-7F (38) 09-0F  text play field        map: 28-5F show: 42-46 (5)
; mode 7 (a3 hires) lines 80-A7 (28)        hires map lower field  map: 60-87
; mode 1 (text)     lines A8-BF (18) 15-17  text status display
; REVISED plan, does not hide anything under the playfield, and make the playfield bigger
; Screen layout:
; mode 1 (text)     lines 00-0F (10) 00-01  score
; mode 6 (bw hires) lines 10-1F (10)        b/w map display
; mode 7 (a3 hires) lines 20-3F (20)        hires map upper field  map: 00-1F(27)
; mode 1 (text)     lines 40-87 (48) 08-11  text play field        map: 20-28
; mode 7 (a3 hires) lines 88-A7 (20)        hires map lower field  map: 29-48(50)
; mode 1 (text)     lines A8-BF (18) 15-17  text status display
; Define the screen region; mode is a display mode, length is number of HBLs.
; nudge is 0 if no nudge, else pos or neg depending on which nudge count to use
ScrRegLen:  .byte   $0E, $0E, $1E, $46, $1E, $16, $00
ScrRegMode: .byte   $01, $06, $07, $01, $07, $01, $00
ScrNudge:   .byte   $00, $80, $01, $00, $01, $00, $00
NudgePos:   .byte   0
NudgeNeg:   .byte   0
PlayX:      .byte   0

; I played with ScRegLen by trial and error a little.
; Not sure why I needed to go two down for region zero.
; Probably because the VBL code (or HBL code) takes long enough that we miss
; some HBLs here and there.

ZFontDots   = $80   ; ZP cache for FontDots to speed up drawing
ZFontCol    = $B0   ; ZP cache for FontCol to speed up drawing
ZBufCount   = $7F   ; count for buffering map data

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
            lda ScrNudge, x     ; nudge if this region needs nudging
            beq postnudge       ; do no nudging
            bmi negnudge        ; using negative (alt) nudge?
            lda NudgePos        ; nope, use the positive (regular) nudge value
            jmp gonudge
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
            bpl keyreturn       ; no key pressed, return (could that even happen? modifier only?)
            sta $0400           ; put it in the corner so I can see it
            sta KeyCaught       ; tell event loop to process this
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
            lda #$1A            ; ZP to $1A00 (standard for interpreters, should already be here)
            sta R_ZP
            jsr herofont        ; load game font into character RAM
            ldx #$27            ; pull the FontDots and FontCol info into ZP for faster access
:           lda FontDots, x
            sta ZFontDots, x
            lda FontCol, x
            sta ZFontCol, x
            dex
            bpl :-
            jsr drawback        ; draw static background
            jsr buildmap        ; set up map
            jsr setupenv        ; arm interrupts
            lda #$00
            sta ScrRegion
            lda #$00            ; start at line 60 of the map
            sta CurrMap
            lda #$10            ; start playfield kind of in the middle
            sta PlayX
            lda #$00            ; start at nudge 0
            sta NudgePos
            sta NudgeNeg
            lda #$00
            sta GameLevel
            lda #$00
            sta GameScore
            lda #$01
            sta RedrawMap       ; start by assuming we need to redraw whole thing
            sta RedrawPlay      ; start by assuming we need to redraw whole thing
            bit IO_KEYCLEAR
            cli                 ; all set up now, interrupt away
eventloop:  lda ExitFlag
            bne alldone
            lda KeyCaught
            beq :+
            jsr handlekey
:            
            inc GameScore + 2   ; DEBUG - constantly move score up
            jsr drawscore
            lda RedrawPlay      ; screen moved, at least playfield needs update
            beq :+
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

; process keypress

handlekey:  
            cmp #$C9            ; I (up, scroll map down)
            bne keym
            inc RedrawPlay      ; need to redraw playfield
            dec NudgePos        ; first try scrolling just by decreasing the nudge
            bpl keydone
            lda #$07            ; if we ran off the top of what we drew
            sta NudgePos        ; reset nudge
            lda CurrMap         ; and subtract 8 from CurrMap
            sec
            sbc #$08
            sta CurrMap
            inc RedrawMap       ; need to redraw whole map
            jmp keydone
keym:       cmp #$CD            ; M (down, scroll map up)
            bne keyj
            inc RedrawPlay      ; need to redraw playfield
            inc NudgePos        ; first try scrolling just by increaing the nudge
            lda NudgePos
            cmp #$08
            bne keydone
            lda #$00            ; if we ran off the bottom of what we drew
            sta NudgePos        ; reset nudge
            lda CurrMap         ; and add 8 to CurrMap
            clc
            adc #$08
            sta CurrMap
            inc RedrawMap       ; need to redraw whole map
            jmp keydone
keyj:       cmp #$CA            ; J (left)
            bne keyk
            dec PlayX
            lda PlayX
            cmp #$FF
            bne :+
            inc PlayX
:           inc RedrawPlay      ; need to redraw playfield
keyk:       cmp #$CB            ; K (right)
            bne keye
            inc PlayX
            lda PlayX
            cmp #$19
            bne :+
            dec PlayX
:           inc RedrawPlay      ; need to redraw playfield
keye:       cmp #$C5            ; E (exit)
            bne keydone            
            inc ExitFlag        ; tell event loop we are exiting
keydone:    lda #$00
            sta KeyCaught
            rts

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

; compute map pointer, based on A.  Map data is in bank 2, $1000-4FFF.
; If CurrMap is something like 00000101 (5), shift bits to translate to:
; MapPtrL: 01000000 (40) MapPtrH: 00010001 (11) ($1140 and $40 bytes there)

setmapptr:  pha
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

; REVISED plan, does not hide anything under the playfield, and make the playfield bigger
            ; Screen layout:
            ; mode 1 (text)     lines 00-0F (10) 00-01  score
            ; mode 6 (bw hires) lines 10-1F (10)        b/w map display
            ; mode 7 (a3 hires) lines 20-3F (20)        hires map upper field  map: 00-1F(27)
            ; mode 1 (text)     lines 40-87 (48) 08-10  text play field        map: 20-26
            ; mode 7 (a3 hires) lines 88-A7 (20)        hires map lower field  map: 27-48(50)
            ; mode 1 (text)     lines A8-BF (18) 15-17  text status display

; the top hires region occupies "lines $20 to $47" but to accommodate nudging we go to $4F
; the middle text section obscures $38 map lines, we display the middle 5 (N+$42 to N+$36)
; the text playfield section is always redrawn and is not nudged. Its top map line is N+$42+nudge.
; characters 01-0F (walls and disk) have color information
; MapColors provides a mapping between 2-bit color and displayed color
; in the hires map, we will use that color for those elements

; TODO -- need to handle display going past the map (draw zeros), since otherwise cannot
; even reach most of the map in the playfield.

scrupdate:
            ; update parts of the screen that need updating.
            ; redrawPlay is set if the playfield in the middle needs updating
            ; redrawMap is set if the map above and below playfield needs updating too
            ; The map can be moved by NudgePos without redrawing, while the playfield cannot.
            ; CurrMap is the top map line of the top map we are looking at.
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
            jmp redrawplay      ; if we only need to redraw playfield, jump there
redrawmap:
            lda #$20            ; top field starts at display line $20
            sta CurScrLine      ; CurScrLine is the current actual line on the screen
            lda CurrMap
            sta $0401           ; DEBUG
            jsr setmapptr       ; load mapptr for CurrMap
hiresline:
            ; prepare ground for pushing to this hires raster line
            ldx CurScrLine
            lda YHiresHA, x     ; get 2000-based address of current line on screen
            ; engineer it so that ZOtherZP in hgr pages always points to the other ZP to flip quickly.
            sta ZOtherZP        ; store HGR1 page in 1A00 ZP.
            sta R_ZP            ; switch to HGR page 1 ZP
            pha                 ; stash it for putting in other ZP's ZOtherZP.
            clc
            adc #$20            ; second page is $2000 higher than first
            sta ZOtherZP        ; store HGR2 2 ZP in HGR1's ZP
            sta R_ZP            ; go to HGR2 ZP
            pla
            sta ZOtherZP        ; recall and store HGR1's ZP in HGR2's ZP
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
            ; we have 64 map data bytes, will draw them over 128 pixels.
            ; which really means drawing 63 bytes over 126 pixels.
            ; using 4 bytes to represent 14 pixels and 7 map data bytes.
            ; mapbytes: 0  7  14  21  28  35  42  49  56  (63) (ZCurrMapX)
            ; pixbytes: 0  4   8  12  16  20  24  28  32  (36) (ZCurrDrawX)
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
            ; buffer in the stack the seven map elements we will represent
            ; read them from right to left, then we draw them from left to right
toplineseg:
            lda #$06            ; we will buffer seven map elements
            sta ZBufCount
            ldy ZCurrMapX
bufmap:     lda (ZScrHole), y
            ; now that we have the byte from the map, we can tranlate this into
            ; the two pixels it will be displaying.
            ; this information comes from FontDots, which we cached into ZFontDots (1A ZP)
            pha                 ; stash the map byte
            and #$30            ; test to see if this is 0-F (separate color info in two high bits)
            beq :+              ; branch if this is an element with an indexed color
            pla                 ; recall map byte, an element with an intrinsic color (from ZFontDots)
            tax
            lda ZFontDots, x    ; look up the color dots for this element
            jmp bufmappix       ; proceed to push
:           pla                 ; recall the map byte to isolate the color
            pha                 ; re-stash the map byte
            and #$C0            ; isolate the color (top two bits)
            clc
            rol
            rol
            rol                 ; convert bits 6/7 to bits 0/1 (color 0-3)
            tax
            lda MapColors, x    ; load the indexed color
            sta ZPxScratch      ; stash the color we found
            pla                 ; get the pixel data back (generally FF or 00 for two pixels)
            and ZPxScratch      ; apply the color
bufmappix:  pha                 ; push buffered pixels onto the stack (safe from ZP switch)
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
            
            lda ZCurrMapX
            bmi :+          ; we had run off the left edge of the line, so now we are done
            lda ZCurrDrawX
            sec
            sbc #$04
            sta ZCurrDrawX
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
            cmp #$B0            ; last line of bottom field complete? ($88-$AF) (8 extra)
            beq hiresdone       ; if so, move to the next screen region
            cmp #$48            ; last line of top field complete? ($20-$47) (8 extra)
            beq redrawplay      ; if so, move on to the middle playfield region
            jmp hiresline       ; otherwise, do the next line
hiresdone:
            jmp lowstats
redrawplay:
            ; the middle lores field starts at map $42 and draws to $46 (plus NudgePos)
            ; TODO - should be some boundary check on movement to handle overflow potential here
            ; TODO - once you can move left and right should draw frame sides as well.
            ; plan: reserve 4 bytes for frame, so display is only covering 36 bytes of the map
            ; frame will be 1...3 for x between 0 and 19
            ; frame will be 2...2 for x between 20 and 41
            ; frame will be 3...1 for x between 43 and 63
            ; or something like that.  Gives an indication of how far over you are.
            ; might also subdivide frame using the last font characters for more granular effect
            ; might also want to indicate somewhere else on the hires as well, like a scroll bar
            lda #$08
            sta CurScrLine
            ; start with top and bottom borders, just colors, chars will already be there
            ; for now just mark horizontal with a color change, but maybe later make it a scrollbar
            tay
borderh:    lda YLoresL, y
            clc
            adc #$27
            tax
            lda YLoresHB, y     ; $800 base (color space)
            sta R_ZP            ; go to color memory
            ldy #$27            ; draw $28 colors
            lda #$50            ; grey1 background
:           sta Zero, x
            dex
            dey
            bmi :+
            cpy PlayX
            bne :-
            lda #$A0            ; grey2 background
            bne :-
:           lda CurScrLine
            cmp #$09            ; if we have done both top and bottom
            beq innerplay       ; move on to the middle
            inc CurScrLine      ; set exit condition for next time
            ldy #$10            ; do the bottom line
            bne borderh         ; branch always
innerplay:
            lda #$1A
            sta R_ZP            ; back to 1A00 ZP
            lda CurrMap
            clc
            adc #$20
            adc NudgePos
            sta $0403
            jsr setmapptr
loresline:
            lda MapPtrL
            sta ZScrHole
            lda MapPtrH
            sta ZScrHole + 1
            lda #$82
            sta ZScrHole + XByte
            ; (ZScrHole), 0 is now the left side of the map data line
            ; buffer all diplayed map bytes into the stack, from right to left
            lda #$27
            sta ZPxScratch
            lda PlayX
            clc
            adc #$27
            tay
loreschar:  lda (ZScrHole), y   ; load map data
            pha                 ; store displayed character on stack
            and #$30            ; test to see if this is 0-F (separate color info)
            beq :+              ; branch if this is an element with an indexed color
            pla                 ; recall character
            pha                 ; re-push character
            tax                 ; this is an element with an intrinsic color (in ZFontCol)
            lda ZFontCol, x     ; so look it up
            jmp gotcolor
:           pla                 ; recall character
            pha                 ; re-push character
            and #$C0            ; isolate the color bits
            clc
            rol
            rol
            rol                 ; convert bits 6/7 to bits 0/1 (color 0-3)
            tax
            pla                 ; recall character
            and #$3F            ; strip the color bits
            pha                 ; and re-push
            lda PlayColors, x   ; load the indexed color
gotcolor:   and #$0F            ; keep only the foreground color (background = black/0)
            ldx ZPxScratch
            sta Zero, x         ; store color in ZP (1A00)
            dey
            dec ZPxScratch
            bpl loreschar
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
            ldx #$27
:           lda Zero, x
            pha
            dex
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
            cmp #$10
            beq :+
            jmp loresline       ; more lines to draw, go draw them
:
            ; if we only needed to redraw the playfield we are done
            lda RedrawMap
            bne :+
            jmp lowstats
            
            ; top and middle fields now drawn, go back and do the bottom one
            ; move the map pointer to start of bottom one
:           lda CurrMap
            clc
            adc #$27
            jsr setmapptr
            ; switch to the lower hires field and draw it from line $88
            lda #$88
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
            sta R_ZP        ; request smallest RTC byte
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
            .byte C_SPACE, C_DISK, C_SPACE, C_SPACE, C_SPACE, C_WALL_V
            .byte C_SPACE, C_SPACE, C_SPACE, C_WALL_R, C_WALL_H, C_WALL_LUD
            .byte C_SPACE, C_SPACE, C_SPACE, C_SPACE, C_SPACE, C_WALL_V
            .byte C_SPACE, C_SPACE, C_SPACE, C_SPACE, C_SPACE, C_WALL_V
            .byte C_WALL_R, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_LU
            ; box open rightward
            .byte C_WALL_RD, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_L
            .byte C_WALL_V, C_SPACE, C_SPACE, C_SPACE, C_SPACE, C_SPACE
            .byte C_WALL_RUD, C_WALL_H, C_WALL_L, C_SPACE, C_SPACE, C_SPACE
            .byte C_WALL_V, C_SPACE, C_SPACE, C_SPACE, C_SPACE, C_SPACE
            .byte C_WALL_V, C_SPACE, C_SPACE, C_DISK, C_SPACE, C_SPACE
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
            
MFX:        .byte 0
MFY:        .byte 0
MFColor:    .byte 0
MFBoxIndex: .byte 0
MFPlaced:   .byte 0
    
buildmap:   lda R_ZP
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
            ldy #$00
            lda C_SPACE
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
            sta MFY
mfbox:      ldx Seed
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
            ; rotate low bits of MFY into high bits of ZPtrA(L)
            ; then add $1000 and store in ZPtrA(H)
            ; so: if map coordinate MFY were $21, shift it so we have
            ; MFY: 0010 0001 -> Zptr: 0100 0000 0001 1000 (40 18 -> $1840)
            ; which is effectively multiplying Y by $40 and adding $1000
            ; then add MFX to get the horizontal offset
            ; ZPtrA will then point to the upper left corner of box target
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
mfpattrow:  lda MFBoxIndex      ; x points to the row of the box pattern
            clc
            adc #$05
            tax
:           lda BoxPatt, x
            ora MFColor
            sta (ZPtrA), y
            dex
            dey
            bpl :-
            ; done if we have completed 6 rows now
            dec MFPlaced
            bmi :+
            ; reset horizontal index to the end of the pattern
            ldy #$05
            ; move to next pattern row
            lda MFBoxIndex
            clc
            adc #$06
            sta MFBoxIndex
            ; move to the next map row
            lda ZPtrA
            clc
            adc #$40
            sta ZPtrA
            bcc mfpattrow
            inc ZPtrA + 1
            bcs mfpattrow
:
            lda MFX             ; move to next X coordinate
            cmp #$39            ; is this the last X coordinate on the line?
            beq :+
            clc
            adc #$08
            sta MFX
            jmp mfbox
:
            lda #$01            ; back to left side
            sta MFX
            lda MFY             ; move to next Y coordinate
            cmp #$F9            ; is the the last Y coordinate on the map?
            beq :+
            clc
            adc #$08
            sta MFY
            jmp mfbox
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

FontDemo:   .byte C_SPACE, C_WALL_R, C_WALL_RD, C_WALL_RU
            .byte C_WALL_RUD, C_WALL_H, C_WALL_L, C_WALL_LD
            .byte C_WALL_LU, C_WALL_LUD, C_WALL_V, C_WALL_U
            .byte C_WALL_D, C_WALL_LRU, C_WALL_LRD, C_DISK
            .byte C_HERO, C_HHEADA, C_HHEADB, C_HHANDUA
            .byte C_HHANDUB, C_HHANDDA, C_HHANDDB, C_HHANDRA
            .byte C_HHANDRB, C_HHANDLA, C_HHANDLB, C_DRIVEL
            .byte C_DRIVER, C_DRIVEU, C_DRIVED, C_FLUXA
            .byte C_FLUXB, C_TRUCKLA, C_TRUCKLB, C_TRUCKRA
            .byte C_TRUCKRB, C_UNUA, C_UNUB, C_UNUC

FontDemoC:  .byte $D0, $F0, $E0, $F0, $E0, $F0, $F0, $C0, $F0, $F0
            .byte $C0, $F0, $E0, $F0, $E0, $F0, $F0, $A0, $B0, $C0
            .byte $D0, $E0, $90, $F0, $F0, $F0, $F0, $F0, $F0, $F0
            .byte $C0, $F0, $E0, $F0, $E0, $F0, $F0, $A0, $B0, $C0

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
            lda FontDemo, y
            sta $7D0, y
            lda FontDemoC, y
            sta $BD0, y
            dey
            bpl :-
            ; text lines 08-11: playfield (frame)
            ldy #$27
:           lda FrameText, y
            sta $428, y
            sta $450, y
            lda FrameCol, y
            sta $828, y ; 
            sta $850, y ; 
            lda InnerText, y
            sta $4A8, y
            sta $528, y
            sta $5A8, y
            sta $628, y
            sta $6A8, y
            sta $728, y
            sta $7A8, y
            lda InnerCol, y
            sta $8A8, y
            sta $928, y
            sta $9A8, y
            sta $A28, y
            sta $AA8, y
            sta $B28, y
            sta $BA8, y
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

CodeEnd     = *