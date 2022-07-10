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

ZDiskType   = $24
ZDiskX      = $26
ZDiskY      = $28
ZHoardX     = $2A
ZHoardY     = $2C
ZHoardXV    = $2E
ZHoardYV    = $30

; we have from A000 to B800 before SOS arrives (6144 bytes)
; I can fudge this a little if needed, by starting with JMP and putting data early,
; since the main concern is trying to run code in a bank switched area.

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
VoidL:      .byte   0
VoidR:      .byte   0
VoidU:      .byte   0
VoidD:      .byte   0

GameLevel:  .byte   0
GameScore:  .byte   0, 0, 0
DisksGot:   .byte   0, 0, 0, 0
DisksLeft:  .byte   0, 0, 0, 0

ScrRegion:  .byte   0
FieldH:     .byte   $04, $05, $05, $06, $06, $07
FieldL:     .byte   $A8, $25, $A8, $28, $A8, $28
FieldHC:    .byte   $08, $09, $09, $0A, $0A, $0B
MapColors:  .byte   $88, $66, $33, $44
PlayColors: .byte   $08, $06, $03, $04
DiskColors: .byte   $0E, $0D, $0C, $0B
; Screen layout:
; mode 1 (text)     lines 00-0F (10) 00-01  score
; mode 6 (bw hires) lines 10-1F (10)        b/w map display
; mode 7 (a3 hires) lines 20-3F (20)        hires map upper field  map: 00-1F
; mode 1 (text)     lines 40-87 (48) 08-10  text play field        map: 20-26
; mode 7 (a3 hires) lines 88-A7 (20)        hires map lower field  map: 27-46
; mode 1 (text)     lines A8-BF (18) 15-17  text status display
;
; Define the screen region; mode is a display mode, length is number of HBLs.
; nudge is 0 if no nudge, else pos or neg depending on which nudge count to use
ScrRegLen:  .byte   $0E, $0E, $1E, $47, $1E, $16, $00
ScrRegMode: .byte   $01, $06, $07, $01, $07, $01, $00
ScrNudge:   .byte   $00, $80, $01, $00, $01, $00, $00
NudgePos:   .byte   0
NudgeNeg:   .byte   0
HeroX:      .byte   0
HeroY:      .byte   0
HeroDir:    .byte   0
VelocityX:  .byte   0
VelocityY:  .byte   0
VBLTick:    .byte   0
MoveDelay   = 1            ; VBL tick delay between moves

; I played with ScRegLen by trial and error a little.
; Not sure why I needed to go two down for region zero.
; Probably because the VBL code (or HBL code) takes long enough that we miss
; some HBLs here and there.
; Also worth noting that it is not fully consistent between MAME and real hardware.
; May want to put some black gaps between switches to fudge that a little, so that
; I don't have to wait for MAME to catch up (and can continue to develop in MAME)
; Since I suspect it is somewhat dependent on workload (some HBLs are missed?) may want
; to wait until it is closer to operational to tweak that.

ZFontDots   = $80   ; ZP cache for FontDots to speed up drawing
ZFontCol    = $B0   ; ZP cache for FontCol to speed up drawing
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
            dec VBLTick
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
            ; point memory at location tracking in bank 2 (with map)
            ; DiskX = 300, DiskY = 340, DiskType = 380
            ; HoardX = 400, HoardY = 440, HoardXV = 480, HoardYV = 4C0
            lda #$00
            sta ZDiskX
            sta ZHoardX
            lda #$40
            sta ZDiskY
            sta ZHoardY
            lda #$80
            sta ZDiskType
            sta ZHoardXV
            lda #$C0
            sta ZHoardYV
            lda #$03
            sta ZDiskX + 1
            sta ZDiskY + 1
            sta ZDiskType + 1
            lda #$04
            sta ZHoardX + 1
            sta ZHoardY + 1
            sta ZHoardXV + 1
            sta ZHoardYV + 1
            lda #$82
            sta ZDiskX + XByte
            sta ZDiskY + XByte
            sta ZDiskType + XByte
            sta ZHoardX + XByte
            sta ZHoardY + XByte
            sta ZHoardXV + XByte
            sta ZHoardYV + XByte
            jsr setupenv        ; arm interrupts
            lda #$00
            sta ScrRegion
            lda #$10            ; start playfield kind of in the middle
            sta HeroX
            lda #$C4            ; Start down near the bottom but at a nudge 0 spot
            sta HeroY
            lda #$00            ; start at nudge 0
            sta NudgePos
            sta NudgeNeg
            sta GameLevel
            sta GameScore
            sta GameScore + 1
            sta GameScore + 2
            sta VelocityX
            sta VelocityY
            sta HeroDir
            lda #MoveDelay       ; set how many VBLs go by before movement advances
            sta VBLTick
            lda #$01
            sta RedrawMap       ; start by assuming we need to redraw whole thing
            sta RedrawPlay      ; start by assuming we need to redraw whole thing
            bit IO_KEYCLEAR
            jsr scrpaint        ; paint the initial screen
            cli                 ; all set up now, interrupt away
eventloop:  lda ExitFlag
            bne alldone
            lda KeyCaught
            beq :+
            jsr handlekey
:            
            inc $480            ; DEBUG - constantly churn screen memory to detect hang
            jsr drawscore
            lda VBLTick
            bpl :+
            jsr domove
            lda #MoveDelay
            sta VBLTick
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

; do moving (called maximally once per MoveDelay VBLs)
; (since otherwise it can be too fast, though this might be a way to make it harder)

; determine where we would move
; check for collision in x alone, y alone, x+y
; if x+y collides with obstacle, but x does not, stop y move x
; otherwise if y does not collide, stop x move y
; otherwise stop (prefers horizontal momentum)
; move if successful

NewHeroX:   .byte   0
NewHeroY:   .byte   0

domove:     ldx HeroX
            lda VelocityX
            beq xchecked
            bmi xleft
            inx
            cpx #$40
            bne xchecked
            dex             ; ran off the right edge, stop horizontal motion
            lda #$00
            sta VelocityX
            beq xchecked
xleft:      dex
            bpl xchecked
            inx             ; ran off the left edge, stop horizontal motion
            stx VelocityX
xchecked:   stx NewHeroX
            ldx HeroY
            txa
            jsr setmapptr   ; locate old hero's y-coordinate on map
            lda MapPtrL
            sta ZPtrB
            lda MapPtrH
            sta ZPtrB + 1
            lda #$82
            sta ZPtrB + XByte
            lda VelocityY
            beq ychecked
            bmi yup
            inx
            bne ychecked
            dex             ; ran off the bottom, stop vertical motion
            stx VelocityY
            beq ychecked
yup:        dex
            cpx #$FF
            bne ychecked
            inx             ; ran off the top, stop vertical motion
            stx VelocityY
ychecked:   stx NewHeroY
            txa
            ; DEBUG - sort of.  Tried to work around a bug where if I get down to
            ; map line FD it halts and I can't move.  If I went straight down, I
            ; seem to be able to escape NW, but it leaves the hero back where I
            ; left it.  So I think I am colliding with the myself somehow and cannot
            ; move away.  Consistently happens when the hero hits FD, which is when
            ; one line of bottom void is showing in the playfield.
            cmp HeroY       ; if we somehow got here but did not move, bail out
            bne xycont
            lda NewHeroX
            cmp HeroX
            bne xycontb
            jmp movenope
xycontb:    lda NewHeroY
            ; below was here before I tried the workaround above.  If that is removed,
            ; it can be removed up to here.
xycont:     jsr setmapptr   ; locate new hero's y-coordinate on map
            lda MapPtrL
            sta ZPtrA
            lda MapPtrH
            sta ZPtrA + 1
            lda #$82
            sta ZPtrA + XByte
            ldy NewHeroX
            lda (ZPtrA), y
            and #$3F        ; color bits don't block movement
            beq movedone
            cmp #C_DISK     ; disk is the only non-obstacle
            beq diagdisk
            ; we have hit something moving in the intended direction
            lda (ZPtrB), y  ; check old Y with new X
            and #$3F        ; color bits don't block movement
            beq horizok
            cmp #C_DISK
            beq horizdisk
            ldy HeroX       ; no go, check new Y with old X
            lda (ZPtrA), y
            and #$3F        ; color bits don't block movement
            beq vertok
            cmp #C_DISK
            beq vertdisk
            ; we have been stopped, move cannot be accomplished
            lda #$00
            sta VelocityX
            sta VelocityY
            jmp movenope
horizdisk:  jsr gotdisk
horizok:    lda #$00            ; stop vertical movement
            sta VelocityY
            lda HeroY           ; new hero Y is unchanged
            sta NewHeroY
            lda ZPtrB           ; map line pointer for new Y is same as old Y
            sta ZPtrA
            lda ZPtrB + 1
            sta ZPtrA + 1
            jmp movedone
vertdisk:   jsr gotdisk
vertok:     lda #$00            ; stop horizontal movement
            sta VelocityX
            lda HeroX           ; new hero X is unchanged
            sta NewHeroX
            jmp movedone
diagdisk:   jsr gotdisk
movedone:   lda #$00            ; remove old hero from map
            ldy HeroX
            sta (ZPtrB), y
            lda #C_HERO         ; put new hero on map
            ldy NewHeroX
            sta (ZPtrA), y
            sty HeroX
            ldy NewHeroY
            sty HeroY
            lda VelocityY       ; scroll the map if we need to
            beq scrollno
            bmi scrolldown
            clc
            bcc scrolldo
scrolldown: sec
scrolldo:   jsr updatemap
scrollno:   jsr drawplay
movenope:   rts

gotdisk:    lda (ZPtrA), y
            and #$C0            ; disk type
            pha
            ora #$20
            lsr
            jsr addscore        ; add type multiplier to the score
            pla
            asl
            rol
            rol
            tax
            sed
            lda DisksGot, x
            clc
            adc #$01
            sta DisksGot, x     ; got one of this type
            lda DisksLeft, x
            sec
            sbc #$01
            sta DisksLeft, x    ; fewer out there of this type
            cld
            ; removing the disk is unnecessary because the hero will replace it
            rts

; process keypress

handlekey:  
            cmp #$D5            ; U (up-left)
            bne :+
            lda #$80
            sta VelocityX
            sta VelocityY
            jmp keydone
:           cmp #$C9            ; I (up, scroll map down)
            bne :+
            lda #$00
            sta VelocityX
            lda #$80
            sta VelocityY
            jmp keydone
:           cmp #$CF            ; O (up-right)
            bne :+
            lda #$01
            sta VelocityX
            lda #$80
            sta VelocityY
            jmp keydone
:           cmp #$CA            ; J (left)
            bne :+
            lda #$80
            sta VelocityX
            lda #$00
            sta VelocityY
            jmp keydone
:           cmp #$CB            ; K (stop)
            bne :+
            lda #$00
            sta VelocityX
            sta VelocityY
            jmp keydone
:           cmp #$CC            ; L (right)
            bne :+
            lda #$01
            sta VelocityX
            lda #$00
            sta VelocityY
            jmp keydone
:           cmp #$CD            ; M (down-left)
            bne :+
            lda #$80
            sta VelocityX
            lda #$01
            sta VelocityY
            jmp keydone
:           cmp #$AC            ; , (down, scroll map up)
            bne :+
            lda #$00
            sta VelocityX
            lda #$01
            sta VelocityY
            jmp keydone
:           cmp #$AE            ; . (down-right)
            bne :+
            lda #$01
            sta VelocityX
            sta VelocityY
            jmp keydone
:           cmp #$C5            ; E (exit)
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

; add to score, add the number in A to the score

addscore:   ldx #$02
            sed
            clc
            adc GameScore, x
            sta GameScore, x
            dex
            lda GameScore, x
            adc #$00
            sta GameScore, x
            dex
            lda GameScore, x
            adc #$00
            sta GameScore, x
            cld
            rts

; put a 2-digit number on screen.
; presumed decimal use of a byte (first nibble 10s, second nibble 1s)
; A holds the number, ZNumPtr holds the screen address of the number.
; Will trigger extended addressing, so set ZNumPtr + XByte to 8F.
; A and Y do not survive.

drawnumber: pha
            lsr
            lsr
            lsr
            lsr
            ora #$30
            ldy #$00
            sta (ZNumPtr), y
            pla
            and #$0F
            ora #$30
            iny
            sta (ZNumPtr), y
            rts

; draw the level and score
; this is fast enough we can just do it whenever

drawscore:
            ; update level
            lda #$07
            sta ZNumPtr
            lda #$04
            sta ZNumPtr + 1
            lda #$8F
            sta ZNumPtr + XByte
            lda GameLevel
            jsr drawnumber
            ; update score
            lda #$16
            sta ZNumPtr
            ldx #$02
:           lda GameScore, x
            jsr drawnumber
            dec ZNumPtr
            dec ZNumPtr
            dex
            bpl :-
            ; update disk types gotten and left
            lda #$D0
            sta ZNumPtr
            lda #$06
            sta ZNumPtr + 1
            ldx #$00
            lda DisksGot, x
            jsr drawnumber
            lda #$D5
            sta ZNumPtr
            lda DisksLeft, x
            jsr drawnumber
            inx
            lda #$DA
            sta ZNumPtr
            lda DisksGot, x
            jsr drawnumber
            lda #$DF
            sta ZNumPtr
            lda DisksLeft, x
            jsr drawnumber
            inx
            lda #$50
            sta ZNumPtr
            lda #$07
            sta ZNumPtr + 1
            lda DisksGot, x
            jsr drawnumber
            lda #$55
            sta ZNumPtr
            lda DisksLeft, x
            jsr drawnumber
            inx
            lda #$5A
            sta ZNumPtr
            lda DisksGot, x
            jsr drawnumber
            lda #$5F
            sta ZNumPtr
            lda DisksLeft, x
            jsr drawnumber
            ; stick a couple of debug indicators on screen
            lda NudgePos
            sta $481
            lda HeroY
            sta $482
            rts

; compute map pointer, based on A.  Map data is in bank 2, $1000-4FFF.
; If current map pointer is something like 00000101 (5), shift bits to translate to:
; MapPtrL: 01000000 (40) MapPtrH: 00010001 (11) ($1140 and $40 bytes there)

setmapptr:  pha
            lda #$00
            sta MapPtrL
            pla
            lsr                 ; shift lower two bits of map line
            ror MapPtrL         ; into higher bits of MPL
            lsr                 ; (multiplying by $40, the length of a map line)
            ror MapPtrL
            clc
            adc #$10            ; map data starts at $1000.
            sta MapPtrH
            rts

; mode 1 (text)     lines 00-0F (10) 00-01  score
; mode 6 (bw hires) lines 10-1F (10)        b/w map display
; mode 7 (a3 hires) lines 20-3F (20)        hires map upper field  map: 00-1F
; mode 1 (text)     lines 40-87 (48) 08-10  text play field        map: 20-26
; mode 7 (a3 hires) lines 88-A7 (20)        hires map lower field  map: 27-46
; mode 1 (text)     lines A8-BF (18) 15-17  text status display

; updatemap will effect a movement of the screen.
; if you call it with carry clear, it will move the map up (hero downward)
; if you call it with carry set, it will move the map down (hero upward)
; it is assumed that the HeroY coordinate has just been changed, triggering this call.
; it will finish by setting NudgePos correctly so that the interrupt handler will display it right.

; in general, increasing nudge:
; copy graphics line 08 + oldnudge to 00 + oldnudge
; copy graphics line 10 + oldnudge to 08 + oldnudge
; copy graphics line 18 + oldnudge to 10 + oldnudge
; draw map line for graphics line 20 + oldnudge on graphics line 18 + oldnudge
; and then advance nudge and #$07.
; to move back up (decreasing nudge)
; copy graphics line 10 + newnudge to 18 + newnudge
; copy graphics line 08 + newnudge to 10 + newnudge
; copy graphics line 00 + newnudge to 08 + newnudge
; draw map line for graphics line 00 + newnudge on graphics line 00 + newnudge

; parameters for increasing nudge, moving hero down, map up, carry clear on entry
parminc:    .byte   $20         ; first copy target raster line in top field (copies go toward zero)
            .byte   $38         ; raster offset for drawing new upper field line ($20 + $18)
            .byte   $88         ; first copy target raster line of lower field (copies go toward zero)
            .byte   $A0         ; raster offset for drawing new lower field line ($88 + $18)
            .byte   $04         ; map offset back from HeroY for newly drawn line in top field.
; parameters for decreasing nudge, moving hero up, map down, carry set on entry
parmdec:    .byte   $38         ; first copy target raster line in lower field (copies toward higher coords)
            .byte   $20         ; raster offset for drawing new upper field line ($20 + 0)
            .byte   $A0         ; first copy target raster line in lower field (copies toward higher coords)
            .byte   $88         ; raster offset for drawing new lower field line ($88 + 0)
            .byte   $23         ; map offset back from HeroY for newly drawn line in top field.
PTopRastA   = $50
PTopRastD   = $51
PBotRastA   = $52
PBotRastD   = $53
PTopMapOff  = $54

PNudge      = $40
TouchedVoid = $41
MapOffset   = $42

; enter with carry clear to increase nudge (with old NudgePos),
; or carry set to decrease nudge (with new already-decreased NudgePos).
; in other words, nudge should be the lowest of new and old
; HeroY just got incremented (carry clear) or decremented (carry set) prior to calling this
updatemap:  php                 ; stash carry flag
            bcs umdec
            ldy #$04            ; end of parminc parameter block
            bne umparms
umdec:      ldy #$09            ; end of parmdec parameter block
umparms:    ldx #$04
:           lda parminc, y
            sta PTopRastA, x    ; copy into ZP parm block
            dey
            dex
            bpl :-
            lda R_BANK          ; save bank
            sta BankSave        ; (but assume we are already in 1A00 ZP)
            lda #$00            ; go to bank 0, where (hires) graphics memory lives
            sta R_BANK
            sta TouchedVoid     ; reset "touched the void" flag
            adc #$04            ; add 4 (inc, carry was 0) or 5 (dec, carry was 1)
            adc HeroY           ; to HeroY, then
            and #$07            ; do mod 8, to get the nudge/pudge value that is most useful.
            sta PNudge          ; nudge/pudge
            ; do the top field
            lda HeroY           ; find the new data line for the top field
            sec
            sbc PTopMapOff
            bcs notvoid
            inc TouchedVoid     ; we have touched the void in the top field
notvoid:    sta MapOffset       ; store the map offset we will draw top field line from
            lda PTopRastA       ; first raster line processed in copy (inc=top, dec=bottom)
            clc
            adc PNudge          ; newnudge/oldnudge
            plp                 ; restore carry flag
            php                 ; re-stash carry flag
            jsr copylines       ; copy lines that can be copied
            lda PTopRastD       ; raster line that is target for new draw
            clc
            adc PNudge          ; plus nudge
            tax                 ; move raster line to X for drawline
            ldy TouchedVoid     ; if we are in the void, draw the void
            beq :+              ; branch if we are not in the void
            jsr drawvoid
            jmp botfield
:           lda MapOffset
            jsr drawline        ; draw line (map pointer is still in A, raster pointer is in X)
            ; do the bottom field
botfield:   lda PBotRastA       ; first raster line processed in copy (inc=top, dec=bottom)
            clc
            adc PNudge          ; newnudge/oldnudge
            plp                 ; restore carry flag
            php                 ; re-stash carry flag
            jsr copylines       ; copy lines that can be copied
            lda PBotRastD       ; raster line that is target for new draw
            clc
            adc PNudge          ; plus nudge
            tax                 ; move raster line to X for drawline
            lda MapOffset       ; find map line for the bottom field
            clc                 ; by adding $27 to the map line from the top field
            adc #$27
            bcc :+              ; we didn't cross a page boundary, we are not in the void
            dec TouchedVoid     ; if we were in the void before, we're not now.  Else we are.
            beq :+              ; if we're not in the void skip to drawing the line
            jsr drawvoid
            jmp upddone
:           jsr drawline        ; draw line (map pointer is still in A, raster pointer is in X)
upddone:    lda BankSave        ; put the bank back
            sta R_BANK
            lda PNudge          ; update the system NudgePos value for the interrupt handler
            plp
            bcs :+              ; if we were decrementing, came in with carry set, this is it.
            adc #$01            ; otherwise, if we were incrementing, we need to add one.
:           and #$07            ; mod 8
            sta NudgePos
            rts

; copylines uses self-modifying code to quickly copy the three graphics lines
; enter with graphics line (top line plus NudgePos), carry clear for increase nudge, set for decrease nudge
; exits with x still holding the line that would be the target of new draw (was last source)
copylines:  tax
            lda #$02            ; move three lines, count in ZPxScratch
            sta ZPxScratch
            lda YHiresL, x
            sta lmtrga + 1      ; target A low
            sta lmtrgb + 1      ; target B low
            lda YHiresHA, x
            sta lmtrga + 2      ; target A high
            lda YHiresHB, x
            sta lmtrgb + 2      ; target B high
lmnext:     txa
            bcs :+              ; if carry is set we are subtracting
            adc #$08            ; if carry is clear we are adding
            clc                 ; keep carry clear so we know for next line
            bcc lmprep
:           sbc #$08
            sec                 ; keep carry set so we know for next line
lmprep:     tax
            lda YHiresL, x
            sta lmsrca + 1      ; source A low
            sta lmsrcb + 1      ; source B low
            lda YHiresHA, x
            sta lmsrca + 2      ; source A high
            lda YHiresHB, x
            sta lmsrcb + 2      ; source B high
            ldy #$27
lmsrca:     lda $2000, y
lmtrga:     sta $4000, y
lmsrcb:     lda $2000, y
lmtrgb:     sta $4000, y
            dey
            bpl lmsrca
            dec ZPxScratch
            bmi :+              ; done moving lines
            ; prior source becomes new target
            lda lmsrca + 1
            sta lmtrga + 1
            lda lmsrca + 2
            sta lmtrga + 2
            lda lmsrcb + 1
            sta lmtrgb + 1
            lda lmsrcb + 2
            sta lmtrgb + 2
            jmp lmnext
:           rts

; seven magenta pixels for the left and right two bytes in the map region
; and for the void lines
BorderBits: .byte %00010001
            .byte %00100010
            .byte %01000100
            .byte %00001000

; do the hires page lookup and ZP setup, common to drawvoid and drawline
; enter with X holding the target line on the graphics page, assumes we are in 1A00 ZP
; returns with X holding the low byte of the starting/leftmost byte on the line
; also updates ZLineStart in 1A00 ZP and sets up ZOtherZP in all ZPs.
prepdraw:   lda YHiresHA, x     ; get 2000-based address of current line on screen
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
            tax
            rts
            
; enter with X holding the target line on the graphics page, assumes we are in 1A00 ZP
drawvoid:   jsr prepdraw
            lda ZOtherZP        ; HGR1
            sta R_ZP
            lda BorderBits      ; write first byte to even bytes on HGR1
            ldy #$1F            ; fill $20 of them
:           sta Zero, x
            inx
            inx
            dey
            bpl :-
            inx                 ; skip 1 to get to odd bytes
            ldy #$1F            ; fill $20 of them
            lda BorderBits + 2  ; write third byte to odd bytes on HGR1
:           sta Zero, x
            dex
            dex
            dey
            bpl :-
            lda ZOtherZP        ; HGR2
            sta R_ZP
            lda BorderBits + 3  ; write fourth byte to odd bytes on HGR2
            ldy #$1F            ; fill $20 of them
:           sta Zero, x
            inx
            inx
            dey
            bpl :-
            dex                 ; skip back 1 to get to even bytes
            ldy #$1F            ; fill $20 of them
            lda BorderBits + 1  ; write second byte to odd bytes on HGR2
:           sta Zero, x
            dex
            dex
            dey
            bpl :-
            lda #$1A            ; go back to $1A00 ZP
            sta R_ZP
            rts

; enter with X holding the target line on the graphics page
; and A holding the map line we will be drawing there
; drawlineb is a second entry point if the map pointer is already set
; this assumes that 1A00 is the normal ZP we start in, and bank 0 (hgr) is switched in
drawline:   jsr setmapptr       ; load mapptr for map line to draw
drawlineb:  jsr prepdraw
            ; draw border bits
            lda ZOtherZP        ; HGR1
            sta R_ZP
            lda BorderBits + 2
            tay
            lda BorderBits
            pha                 ; store first byte on HGR1 line byte 0
            sta Zero, x
            inx
            tya                 ; store third byte on HGR1 line byte 1
            sta Zero, x
            txa                 ; skip X ahead to the other side of the line
            clc
            adc #$26
            tax
            tya
            sta Zero, x         ; store third byte on HGR1 line byte $27
            dex
            pla
            sta Zero, x         ; store first byte on HGR1 line byte $26
            lda ZOtherZP        ; HGR2
            sta R_ZP
            lda BorderBits + 3
            tay
            lda BorderBits + 1
            pha
            sta Zero, x         ; store second byte on HGR2 line byte $26
            inx
            tya         
            sta Zero, x         ; store fourth byte on HGR2 line byte $27
            txa                 ; skip X back to the left of the line
            sec
            sbc #$26
            tax
            tya
            sta Zero, x         ; store fourth byte on HGR2 line byte 1
            dex
            pla
            sta Zero, x         ; store second byte on HGT2 line byte 0
            lda #$1A            ; go back to 1A00 ZP.
            sta R_ZP
            ; push left edge to the right 7 pixels to center the map fields
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
            ; now that we have the byte from the map, we can translate this into
            ; the two pixels it will be displaying.
            ; this information comes from FontDots, which we cached into ZFontDots (1A ZP)
            pha                 ; stash the map byte
            and #$3F            ; strip any color bits
            tax
            lda ZFontDots, x    ; get the pixels
            sta ZPxScratch      ; stash the pixels
            txa
            and #%00110000      ; test to see if this is 0-F (separate color info in two high bits)
            beq :+              ; branch if this is an element with an indexed color
            pla                 ; throw away the map byte
            lda ZPxScratch      ; these are the final pixels
            jmp bufmappix
:           pla                 ; recall the map byte to grab the color
            asl
            rol
            rol                 ; move color bits into lower two bits to serve as color index
            and #$03
            tax
            lda MapColors, x    ; load the indexed color
            and ZPxScratch      ; apply to the pixels
bufmappix:  pha                 ; push buffered pixels onto the stack (safe from ZP switch)
            dey
            dec ZBufCount
            bpl bufmap
            sty ZCurrMapX       ; save new pointer for end of next (to the left) block after this
            ; the pixels have now been translated, we can send them to the screen
            ; the 7 pixels on the stack each use 8 bits, but we need to smear them across the
            ; 8 bytes of graphics memory using 7 bits at a time.  I know, right?
            ;
            ; As per the Apple /// Level 2 Service Reference Manual:
            ; There are two distinct screen pages in this mode but the mapping of the
            ; individual pages is, at first encounter, a bit difficult to master. Good
            ; luck!
            ;  o The display dot represents a sequence of 4 data bits in the RAM
            ;    display area.
            ;  o Two rams are used starting at 2000 and 4000 respectively and alternate
            ;    bytes are fetched from each ram area.
            ;  o In any video mode only 7 of the 8 bits of each byte are displayed
            ; With this information in mind...and remembering that each pixel in this mode
            ; is made from 4 bits...you can see that you need 4 bytes of information to get
            ; 7 pixels.  The way in which these bytes may into picture elements is shown
            ; below.
            ;
            ; [conspiracy-corkboard.jpg]
            ;
            ; |   2000      |   4000      |   2001      |   4001      |
            ; | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
            ; |LSB       MSB|             |             |             | 
            ; |-- P1 -|-- P2 -|-- P3 -|-- P4 -|-- P5 -|-- P6 -|-- P7 -|
            ;
            ; end quote
            ; 
            ; Colors:
            ; 0000 0 black      0100 4 darkgreen    1000 8 brown    1100 C green
            ; 0001 1 magenta    0101 5 grey1        1001 9 orange   1101 D yellow
            ; 0010 2 darkblue   0110 6 medblue      1010 A grey2    1110 E aqua
            ; 0011 3 purple     0111 7 lightblue    1011 B pink     1111 F white
            ;
            ; The bits increase steadily in significance from pixel 1 to pixel 7.
            ; LSB->MSB
            ;  1000100  0100010  0010001  0001000
            ; MSB->LSB
            ;  0010001  0100010  1000100  0001000
            ; 00010001 00100010 01000100 00001000
            ldx ZCurrDrawX      ; set x to the horizontal offset on screen
            lda ZOtherZP        ; go to HGR1 ZP for drawing
            sta R_ZP
            ; byte 0 (byte 0 page 1): -1110000 [0+0] 421/8421
            pla                 ; pixels 0-1
            ;lda #$21            ; DEBUG troubleshoot blit
            pha                 ; remember for later
            and #$7F
            sta Zero, x
            ; byte 1 (byte 0 page 2): -3322221 [0+1+1] 21/8421/8
            pla                 ; recall color of pixel 1
            asl                 ; move hi bit of pixel 1 color
            rol                 ; into lo bit of byte 1
            and #$01
            sta ZPxScratch      ; stash bit of pixel 1
            pla                 ; pixels 2-3
            ;lda #$43            ; DEBUG troubleshoot blit
            pha                 ; remember for later
            asl                 ; move pixel 2's and 3's bits up
            and #%011111110     ; and chop off the two hi bits of pixel 3
            ora ZPxScratch
            ; put this pixel data on the other ZP (page 2)
            ldy ZOtherZP
            sty R_ZP            ; go to page 2 ZP
            sta Zero, x
            ldy ZOtherZP
            sty R_ZP            ; go to page 1 ZP
            inx
            ; byte 2 (byte 1 page 1): -5444433 [1+2+2] 1/8421/84
            pla                 ; recall color of pixel 3
            asl
            rol
            rol                 ; put pixel 3's hi bits in low bits
            and #$03            ; isolate the pixel 3 color's higher two bits
            sta ZPxScratch      ; and stash them
            pla                 ; pixels 4-5
            ;lda #$65            ; DEBUG troubleshoot blit
            pha                 ; remember for later
            asl
            asl
            ora ZPxScratch
            and #$7F
            sta Zero, x
            ; byte 3 (byte 1 page 2): -6666555 [2+3] 8421/842
            pla                 ; recall color of pixel 5
            asl                 ; move higher 3 bits of pixel 5 into low 3 bits
            rol
            rol
            rol
            and #$07
            sta ZPxScratch
            pla                 ; pixels 6-7
            ;lda #$87            ; DEBUG troubleshoot blit
            pha                 ; remember for later
            asl
            asl
            asl
            ora ZPxScratch
            and #$7F
            ; put this data on the page 2 ZP
            ldy ZOtherZP
            sty R_ZP            ; go to page 2 ZP
            sta Zero, x
            ldy ZOtherZP
            sty R_ZP            ; go to page 1 ZP
            inx
            ; byte 4 (byte 2 page 1): -8887777 [3+4] 421/8421
            pla                 ; recall color of pixel 7
            lsr
            lsr
            lsr
            lsr
            sta ZPxScratch
            pla                 ; pixels 8-9
            ;lda #$A9            ; DEBUG troubleshoot blit
            pha                 ; remember for later
            asl
            asl
            asl
            asl
            ora ZPxScratch
            and #$7F
            sta Zero, x
            ; byte 5 (byte 2 page 2): -AA99998 [4+4+5]  21/8421/8
            pla                 ; recall color of pixels 8 and 9
            lsr
            lsr
            lsr
            sta ZPxScratch
            pla                 ; pixels A-B
            ;lda #$CB            ; DEBUG troubleshoot blit
            pha                 ; remember for later
            lsr
            ror
            ror
            ror
            and #%01100000
            ora ZPxScratch
            ; put this data on the other ZP
            ldy ZOtherZP
            sty R_ZP            ; go to page 2 ZP
            sta Zero, x
            ldy ZOtherZP
            sty R_ZP            ; go to page 1 ZP
            inx
            ; byte 6 (byte 3 page 1): -CBBBBAA [5+5+6] 1842184
            pla                 ; recall color of pixel A
            lsr
            lsr
            and #%00111111
            sta ZPxScratch
            pla                 ; pixels C-D
            ;lda #$ED            ; DEBUG troubleshoot blit
            pha                 ; remember for later
            lsr
            ror
            and #%01000000
            ora ZPxScratch
            sta Zero, x
            ; byte 7 (byte 3 page 2): -DDDDCCC [6+6] 4218421
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
:           rts

; draw the (text based) playfield in the middle

BorderR:    .byte 0
BorderV:    .byte 0
BorderRYet: .byte 0

BorderChar  = $00       ; C_SPACE
BorderColA  = $AF       ; grey2 background
BorderColB  = $5F       ; grey1 background

; translation table between text column and hires map column, when drawing passes from right
; scrollbar thumb toggles state
ThumbXlate: .byte   $80, $80
            .byte   $01, $03, $05, $07
            .byte   $08, $0A, $0C, $0E
            .byte   $0F, $11, $13, $15
            .byte   $16, $18, $1A, $1C
            .byte   $1D, $1F, $21, $23
            .byte   $24, $26, $28, $2A
            .byte   $2B, $2D, $2F, $31
            .byte   $32, $34, $36, $38
            .byte   $39, $3B, $3D, $3F
            .byte   $80, $80
ThumbTogg:  .byte   $00
ThumbNext:  .byte   $00
PlayLeft:   .byte   $00
PlayRight:  .byte   $00

drawplay:   
            ; the middle lores field starts at map $42 and draws to $46 (plus NudgePos)
            ; in order to keep hero in the middle, five columns are used by a frame
            ; based on hero position, 5 total, high nibble of HeroX of those are on the right
            ; (i.e. if HeroX is 32, there are 3 on the right, 2 on the left)
            lda #$08
            sta CurScrLine
            ; compute border columns (0-based count of columns on the right is high nibble of HeroX)
            lda HeroX
            lsr
            lsr
            lsr
            lsr
            sta BorderR
            ; compute thumb boundaries
            lda HeroX
            clc
            adc #$11
            sta ThumbTogg
            sta PlayRight
            lda HeroX
            sec
            sbc #$11            ; could derive left void from this but might not be faster
            bpl :+
            lda #$00
:           sta PlayLeft
            sta ThumbNext
            ; compute the voids (areas in the display but off the edges of the map)
            lda #$11
            sec
            sbc HeroX
            bpl :+              ; there is a left void
            lda #$00
:           sta VoidL
            lda HeroX
            sec
            sbc #$2E
            bpl :+              ; there is a right void
            lda #$00
:           sta VoidR
            lda HeroY           ; check for top void, only care if HeroY is in the top half of map
            bmi novoidu         ; branch if HeroY is in the bottom half of the map
            lda #$03
            sec
            sbc HeroY
            bpl :+              ; there is a top void
novoidu:    lda #$00
:           sta VoidU
dppostvoid: ; start with top and bottom borders, just colors, chars will already be there
            ldy CurScrLine
borderh:    lda YLoresL, y
            clc
            adc #$27
            tax
            lda YLoresHB, y     ; $800 base (color space)
            sta R_ZP            ; go to color memory
            ldy #$27            ; draw $28 colors
            lda #$57            ; grey1 background
borderhb:   sta Zero, x
            dex
            dey
            bmi borderhz
            pha
            lda ThumbXlate, y   ; what map column are we entering?
            bmi borderfix       ; this column has a fixed color
            cmp ThumbTogg       ; did we just pass the toggle?
            bcc borderhtog      ; yes
            pla
            bne borderhb        ; branch always
borderfix:  pla                 ; throw away the color
            lda #$57            ; brighter gray
            bne borderhb
borderhtog: lda ThumbNext       ; arm new left side toggle
            sta ThumbTogg
            lda #$00            ; disable left side toggle
            sta ThumbNext
            pla                 ; and swap colors
            eor #$A7
            bne borderhb        ; branch always
borderhz:   lda CurScrLine
            cmp #$09            ; if we have done both top and bottom
            beq innerplay       ; move on to the middle
            lda PlayRight
            sta ThumbTogg
            lda PlayLeft
            sta ThumbNext
            lda #$50
            inc CurScrLine      ; set exit condition for next time (borderh does not use the value)
            ldy #$10            ; do the bottom line (Y holds the current screen line for borderh)
            bne borderh         ; branch always
innerplay:  lda HeroY           ; find map pointer for top non-void line
            clc
            adc VoidU           ; factor out upper void
            sec
            sbc #$03
            jsr setmapptr
burnvoidu:  dec VoidU           ; burn through upper void lines first if there are any
            bmi pfmapstart
            jsr playvoid
            inc CurScrLine
            bne burnvoidu       ; branch always
pfmapstart: lda #$1A
            sta R_ZP            ; back to 1A00 ZP
pfline:     jsr loresline            
            lda MapPtrL         ; advance map pointer
            clc
            adc #$40
            sta MapPtrL
            bcc :+
            inc MapPtrH
            lda MapPtrH
            cmp #$50
            beq burnvoidd       ; just ticked into a lower void
            ; advance screen line
:           inc CurScrLine
            lda CurScrLine
            cmp #$10
            bne pfline          ; more lines to draw, go draw them
pfdone:     rts
burnvoidd:  inc CurScrLine
            lda CurScrLine
            cmp #$10
            beq pfdone
            jsr playvoid
            jmp burnvoidd

; draw a void line in the playfield
; TODO - this doesn't reflect borders right, may want to unfactor this out
playvoid:   lda #$04            ; draw five border columns total
            sta BorderV
            lda BorderR         ; save a local copy of this that we can decrement
            sta BorderRYet
            ldy CurScrLine
            lda YLoresL, y
            clc
            adc #$27            ; compute index of right edge of line
            pha                 ; save for second pass as well
            tax
            lda YLoresHB, y     ; $800 base (color space)
            pha                 ; stash color space page
            lda YLoresHA, y     ; $400 base (char space)
            sta R_ZP            ; go to character memory
            ldy #$27
            lda #BorderChar
:           sta Zero, x
            dex
            dey
            dec BorderV
            dec BorderRYet
            bpl :-
            lda #C_SPACE
:           sta Zero, x
            dex
            dey
            cpy BorderV
            bne :-
            lda #BorderChar
:           sta Zero, x
            dex
            dey
            bpl :-
            lda #$04
            sta BorderV
            lda BorderR
            sta BorderRYet
            pla                 ; recall color space page
            sta R_ZP            ; go to color memory
            pla                 ; recall index of right edge of line
            tax
            ldy #$27
            lda #BorderColB
:           sta Zero, x
            lda #BorderColA     ; switch to darker gray
            dex
            dey
            dec BorderV
            dec BorderRYet
            bpl :-
            lda #$10            ; magenta background, black foreground
:           sta Zero, x
            dex
            dey
            cpy BorderV
            bne :-
            lda #BorderColA
:           sta Zero, x
            dex
            dey
            bpl :-
            lda #BorderColB     ; replace last one with lighter gray
            inx
            sta Zero, x
            rts

; draw a lores line in the playfield, assumes ZP is 1A00 and MapPtr is set.
loresline:  lda MapPtrL
            sta ZScrHole
            lda MapPtrH
            sta ZScrHole + 1
            lda #$82
            sta ZScrHole + XByte
            ; (ZScrHole), 0 is now the left side of the map data line
            ; buffer all diplayed map bytes into the stack, from right to left
            lda #$04            ; draw five border columns total
            sta BorderV
            lda BorderR         ; save a local copy of this that we can decrement
            sta BorderRYet
            ldx #$27
            ; draw the right border
            lda #BorderChar
            ldy #BorderColB
:           pha                 ; push the border character
            pha                 ; push it again because we want to recall it later
            tya
            sta Zero, x         ; store color
            ldy #BorderColA     ; switch color to darker gray
            pla                 ; recall character for next push
            dex                 ; decrement drawn x coordinate
            dec BorderV         ; we've drawn one border element
            dec BorderRYet      ; we've drawn one of the right side border elements
            bpl :-              ; we have not yet drawn ALL of the right side border elements
            stx ZPxScratch      ; save where the border ended
            lda HeroX           ; find the per-line offset into the map data from the left side of the map
            clc
            adc #$11
            tay
loreschar:  cpy #$40            ; check to see if we're off the right edge of the map
            bcc :+              ; not in a right edge void
            lda #C_SPACE        ; right edge void, push a magenta blank
            pha
            lda #$10            ; magenta
            bne gotcolorb       ; skip over the map part to go to next column
:           lda (ZScrHole), y   ; load map data
            pha                 ; store displayed character on stack
            and #%00110000      ; test to see if this is 0-F (separate color info)
            beq :+              ; branch if this is an element with an indexed color
            pla                 ; recall character
            pha                 ; re-push character
            tax                 ; this is an element with an intrinsic color (in ZFontCol)
            lda ZFontCol, x     ; so look it up
            jmp gotcolor
:           pla                 ; recall character
            pha                 ; re-push character
            asl
            rol
            rol                 ; move color bits into lower two bits to serve as color index
            and #$03
            tax
            pla                 ; recall character
            and #$3F            ; strip the color bits
            pha                 ; and re-push
            cmp #C_DISK         ; if it is a disk, use the disk colors
            bne useplaycol
            lda DiskColors, x
            bne gotcolor        ; branch always
useplaycol: lda PlayColors, x   ; load the indexed color
gotcolor:   and #$0F            ; keep only the foreground color (background = black/0)
gotcolorb:  ldx ZPxScratch
            sta Zero, x         ; store color in ZP (1A00)
            dey
            bmi leftvoid        ; oops, we are about to plummet into the left void
            dec ZPxScratch
            dex
            cpx BorderV
            beq leftborder
            bne loreschar
leftvoid:   dex                 ; we touched the void, any left to draw?
            cpx BorderV
            beq leftborder      ; nope, we have now drawn them all
            lda #C_SPACE        ; left edge void, push a magenta blank - DEBUG
            pha
            lda #$10            ; magenta
            sta Zero, x
            bne leftvoid
            ; draw the left border
leftborder: lda #BorderChar
            ldy #BorderColA
:           pha                 ; push the character
            pha                 ; push it again because we want to recall it later
            tya
            sta Zero, x         ; store color
            pla                 ; recall character for next push
            dex                 ; decrement drawn x coordinate
            dec BorderV         ; we've drawn one border element
            bpl :-              ; we have not yet drawn ALL of the left side border elements
            ; replace last color with lighter gray
            lda #BorderColB
            inx
            sta Zero, x            
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
            lda #$1A
            sta R_ZP            ; go back to 1A00 ZP
            rts

; paint the whole screen - needs to be done once at the beginning so we can
; then use the smooth scroll to move the hires fields around.
scrpaint:
            lda R_ZP
            sta ZPSave          ; save current ZP
            lda R_BANK
            sta BankSave        ; save current bank
            lda #$00            ; swap in bank zero,
            sta R_BANK          ; where (hires) graphics memory lives
            lda #$20            ; top field starts at display line $20
            sta CurScrLine      ; CurScrLine is the current actual line on the screen
            lda HeroY           ; HeroY is the $23rd abstract line down
            sec
            sbc #$23            ; go find where the map data pointer for the top of the top field is
            bcs :+              ; we did not run off the edge, there is no upper void
            eor #$FF            ; invert the negative number to find the size of the upper void
            adc #$01
            tax                 ; put the vertical extent of the upper void in X
            lda #$00            ; start the map at zero when we get past the void
            beq :++
:           ldx #$00            ; no upper void, extent of the upper void is 0
:           stx VoidU           ; number of void lines above the map data
            jsr setmapptr       ; load mapptr for the first map data line we will be drawing
            cpx #$00            ; is there a void?
            beq hiresline       ; no, there is no void, just go start drawing
:           ldx CurScrLine      ; yes, there is a void, load the raster line into X
            jsr drawvoid        ; draw the void line
            inc CurScrLine      ; move down to the next raster line
            dec VoidU           ; if there are still void lines left, keep drawning them
            bpl :-
hiresline:  ldx CurScrLine      ; target line on graphics screen
            jsr drawlineb       ; we already set MapPtr earlier, use internal entry point
            lda MapPtrL         ; advance the map pointer to the next line
            clc
            adc #$40
            sta MapPtrL
            bcc novoid
            inc MapPtrH
            lda MapPtrH
            cmp #$50            ; did we just fall off the map into the bottom void?
            bne novoid          ; no, so continue on
:           inc CurScrLine      ; we are in the bottom void, advance the graphics raster line
            ldx CurScrLine      ; note that if we are in the lower void, we must be in bottom field
            cpx #$A9            ; last line of bottom field complete? ($88-$A8)
            beq redrawplay
            jsr drawvoid
            jmp :-
novoid:     inc CurScrLine      ; advance the graphics raster line
            ldx CurScrLine
            cpx #$A9            ; last line of bottom field complete? ($88-$A8)
            beq redrawplay      ; if so, move to the next screen region
            cpx #$40            ; last line of top field complete? ($20-$3F)
            bne hiresline       ; nope, keep going
            ; we just finished the top field
            lda #$88            ; set the raster line for the start of the lower field
            sta CurScrLine
            lda HeroY           ; set up starting point for lower field
            clc
            adc #$04
            bcs :+              ; are we already in a lower void? If so, the whole field is in the void.
            jsr setmapptr       ; not in the void, set up MapPtr for the data
            jmp hiresline
:           ldx CurScrLine
            jsr drawvoid
            inc CurScrLine
            lda CurScrLine
            cmp #$A9            ; last line of bottom field complete? ($88-$A8)
            bne :-              ; nope, keep drawing void lines
redrawplay: jsr drawplay

            ; draw the last field on the screen, the mode 1 lower status region
            ; TOOD

            lda #$00            ; reset the update flags
            sta RedrawMap
            sta RedrawPlay
            lda ZPSave          ; restore ZP and bank
            sta R_ZP
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

; 5x5 box patterns for adding to the map.  Each pattern is $19 bytes long.
BoxIndex:   .byte 0, 25, 50, 75, 100, 125, 150, 175
BoxPatt:
            ; box open leftward
            .byte   C_WALL_R,   C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_LD
            .byte   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_V
            .byte   C_SPACE,    C_SPACE,    C_WALL_R,   C_WALL_H,   C_WALL_LUD
            .byte   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_V
            .byte   C_WALL_R,   C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_LU
            ; box open leftward
            .byte   C_WALL_R,   C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_LD
            .byte   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_V
            .byte   C_SPACE,    C_SPACE,    C_WALL_R,   C_WALL_H,   C_WALL_LUD
            .byte   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_V
            .byte   C_WALL_R,   C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_LU
            ; box open rightward
            .byte   C_WALL_RD,  C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_L
            .byte   C_WALL_V,   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE
            .byte   C_WALL_RUD, C_WALL_H,   C_WALL_L,   C_SPACE,    C_SPACE
            .byte   C_WALL_V,   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE
            .byte   C_WALL_RU,  C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_L
            ; box open upward
            .byte   C_WALL_D,   C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_D
            .byte   C_WALL_V,   C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_V
            .byte   C_WALL_V,   C_SPACE,    C_WALL_D,   C_SPACE,    C_WALL_V
            .byte   C_WALL_V,   C_SPACE,    C_WALL_V,   C_SPACE,    C_WALL_V
            .byte   C_WALL_RU,  C_WALL_H,   C_WALL_LRU, C_WALL_H,   C_WALL_LU
            ; box open downward
            .byte   C_WALL_RD,  C_WALL_H,   C_WALL_LRD, C_WALL_H,   C_WALL_LD
            .byte   C_WALL_V,   C_SPACE,    C_WALL_V,   C_SPACE,    C_WALL_V
            .byte   C_WALL_V,   C_SPACE,    C_WALL_U,   C_SPACE,    C_WALL_V
            .byte   C_WALL_V,   C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_V
            .byte   C_WALL_U,   C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_U
            ; well open left and right
            .byte   C_WALL_R,   C_WALL_H,   C_WALL_LRD, C_WALL_H,   C_WALL_L
            .byte   C_SPACE,    C_SPACE,    C_WALL_V,   C_SPACE,    C_SPACE
            .byte   C_SPACE,    C_SPACE,    C_WALL_V,   C_SPACE,    C_SPACE
            .byte   C_SPACE,    C_SPACE,    C_WALL_V,   C_SPACE,    C_SPACE
            .byte   C_WALL_R,   C_WALL_H,   C_WALL_LRU, C_WALL_H,   C_WALL_L
            ; box open up and down
            .byte   C_WALL_D,   C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_D
            .byte   C_WALL_V,   C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_V
            .byte   C_WALL_RUD, C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_LUD
            .byte   C_WALL_V,   C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_V
            .byte   C_WALL_U,   C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_U
            ; box open up and right
            .byte   C_WALL_D,   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE
            .byte   C_WALL_V,   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE
            .byte   C_WALL_V,   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE
            .byte   C_WALL_V,   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE
            .byte   C_WALL_RU,  C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_L
            ; box open down and left
            .byte   C_WALL_RD,  C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_L
            .byte   C_WALL_D,   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE
            .byte   C_WALL_D,   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE
            .byte   C_WALL_D,   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE
            .byte   C_WALL_D,   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE
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
            ; every 8x8 cell gets a 5x5 box "centered" in it
            ; since we're always going one down and one in, start at 1, 1
            lda #$01
            sta MFX
            sta MFY
mfbox:      ldx Seed
            lda Random, x   ; pick a random box shape
            and #$07
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
            ldy #$04
            sty MFPlaced        ; do 5 rows
mfpattrow:  lda MFBoxIndex      ; x points to the row of the box pattern
            clc
            adc #$04
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
            ldy #$04
            ; move to next pattern row
            lda MFBoxIndex
            clc
            adc #$05
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
            lda #$30            ; disks to scatter around
            sta MFPlaced
:           jsr mfrndspot
            sty ZPxScratch      ; stash the x-coordinate
            txa
            pha                 ; stash the y-coordinate
            ldx Seed
            lda Random, x
            inx
            stx Seed
            and #$03            ; pick a random type
            tax                 ; stash type in X
            tya                 ; store the x-coordinate
            ldy MFPlaced
            sta (ZDiskX), y
            pla                 ; store the y-coordinate
            sta (ZDiskY), y
            txa                 ; store the type
            sta (ZDiskType), y
            tax
            sed
            lda DisksLeft, x    ; record another one in the map inventory
            clc
            adc #$01
            sta DisksLeft, x
            cld
            lsr
            ror
            ror                 ; convert type to color bits
            ora #C_DISK         ; use disk shape
            ldy ZPxScratch      ; recall the x-coordinate
            sta (ZPtrA), y      ; place a disk
            dec MFPlaced
            bpl :-
            
            lda #$10            ; hoarders to scatter around
            sta MFPlaced
:           jsr mfrndspot
            sty ZPxScratch      ; stash the x-coordinate
            txa
            ldy MFPlaced
            sta (ZHoardY), y
            lda ZPxScratch
            sta (ZHoardX), y
            lda #$01
            sta (ZHoardXV), y
            lda #$00
            sta (ZHoardYV), y
            ldy ZPxScratch      ; place the hoarder
            lda #C_HHEADA
            sta (ZPtrA), y
            iny
            lda #C_HHANDRA
            sta (ZPtrA), y
            dec MFPlaced
            bpl :-

mfdone:     lda ZPSave
            sta R_ZP
            rts

; find a random spot that is open in the map
; returns with y holding the map x coordinate, x holding the map y coordinate,
; and ZPtrA pointing to the map row start
mfrndspot:  ldx Seed
            lda Random, x
            inx
            pha                     ; stash the y coordinate
            jsr setmapptr
            lda Random, x
            inx
            stx Seed
            and #$3F
            tay
            lda MapPtrL
            sta ZPtrA
            lda MapPtrH
            sta ZPtrA + 1
            lda #$82
            sta ZPtrA + XByte
            pla                     ; unstash the y coordinate in case we're done
            tax
            lda (ZPtrA), y
            bne mfrndspot           ; if spot wasn't empty keep looking maybe forever
            iny                     ; spot needs to have space to its right as well
            lda (ZPtrA), y
            bne mfrndspot           ; if spot wasn't empty keep looking maybe forever
            dey
            rts

; paint the static parts of the page

StatTextA:  .byte " Level    "
            .byte " Score:   "
            .byte "          "
            .byte " DISKHERO "

StatColA:   .byte $2D, $2D, $2D, $2D, $2D, $2D, $2E, $2E, $2E, $2E
            .byte $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0D, $0D
            .byte $0D, $0D, $0D, $0D, $0D, $F0, $B0, $E0, $C0, $D0
            .byte $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E

StatTextB:  .byte "        ", C_TRUCKLB, C_TRUCKLA
            .byte "          "
            .byte "       ", C_TRUCKRB, C_TRUCKRA, " "
            .byte " -------- "
            
StatColB:   .byte $D0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0
            .byte $F0, $F0, $F0, $F0, $F0, $F0, $F0, $A0, $B0, $C0
            .byte $D0, $E0, $90, $F0, $F0, $F0, $0E, $0D, $0C, $0E
            .byte $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E

ProgTextA:  .byte "00 ", C_DISK, " 00 1 "
            .byte "00 ", C_DISK, " 00 2 "
            .byte "          "
            .byte "          "

ProgColA:   .byte $0F, $0F, $0F, $0E, $0F, $0E, $0E, $0F, $E1, $0F
            .byte $0F, $0F, $0F, $0D, $0F, $0E, $0E, $0F, $E1, $0F
            .byte $F0, $F0, $F0, $F0, $F0, $F0, $F0, $A0, $B0, $C0
            .byte $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E

ProgTextB:  .byte "00 ", C_DISK, " 00 3 "
            .byte "00 ", C_DISK, " 00 4 "
            .byte "          "
            .byte "          "

ProgColB:   .byte $0F, $0F, $0F, $0C, $0F, $0E, $0E, $0F, $E1, $0F
            .byte $0F, $0F, $0F, $0B, $0F, $0E, $0E, $0F, $E1, $0F
            .byte $F0, $F0, $F0, $F0, $F0, $F0, $F0, $A0, $B0, $C0
            .byte $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E

FrameText:  .byte C_WALL_RUD, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H
            .byte C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H
            .byte C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H
            .byte C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H
            .byte C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H
            .byte C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H
            .byte C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H
            .byte C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_LUD

FrameCol:   .byte $D0, $F0, $F0, $F0, $F0, $F0, $F0, $C0, $F0, $F0
            .byte $F0, $F0, $F0, $F0, $F0, $F0, $F0, $A0, $B0, $C0
            .byte $D0, $E0, $90, $F0, $F0, $F0, $F0, $F0, $F0, $F0
            .byte $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E

InnerText:  .byte C_WALL_V, "         "
            .byte "          "
            .byte "          "
            .byte "         ", C_WALL_V

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
            ; text lines 00-01: score status
            ldy #$27
:           lda StatTextA, y
            sta $400, y
            lda StatColA, y
            sta $800, y
            lda StatTextB, y
            sta $480, y
            lda StatColB, y
            sta $880, y
            dey
            bpl :-
            ; text lines 15-17: progress status
            ldy #$27
:           lda ProgTextA, y
            sta $6D0, y
            lda ProgColA, y
            sta $AD0, y
            lda ProgTextB, y
            sta $750, y
            lda ProgColB, y
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
            ; if it really needs clearing, we can just zero out 2000-9FFF
            ; but I don't think it will need it, so I will skip it.
            rts

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