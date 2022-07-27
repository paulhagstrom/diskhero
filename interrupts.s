; Diskhero
; Interrupt-related routines
;
; see diskhero.inc for ZP definitions (generally trying to stay between D0-FF)
; though I have mostly removed ZP usage except in the audio handler

IntXStash:  .byte   0   ; saved X register from interrupt handler entry
IntYStash:  .byte   0   ; saved Y register from interrupt handler entry

; variables for sound and speed management
VBLTick:    .byte   0   ; ticked down for each VBL, can use to delay things for several refreshes
VBLTickP:   .byte   0   ; playfield ticker, try to draw during VBL
ClockTick:  .byte   0   ; ticked down for each clock-during-VBL, for playing sound (only) during VBL

; Screen regions: (screen splitting definition)
; ScrRegMode is the graphics mode of the region as defined in the table below
;   encoded as a multiple of $0C so that it can be used as a branch/jump table.
; This is set up to split in blocks of 8 scan lines, to do multiples, repeat the mode
; Last mode (only) will use the smooth scroll parameter, also encoded as a branch offset.
; These are in reverse order so that I can quickly detect if it runs off the end somehow
; and reset it to the top.  ScrRegModB is used in resetting, it is "NextMode" when resetting.
ScrRegMode: .byte   $0F, $0F, $2D, $2D, $2D, $2D, $00, $00
            .byte   $00, $00, $00, $00, $00, $00, $00, $00
            .byte   $2D, $2D, $2D, $2D, $00, $00 
ScrRegModB: .byte   $1E, $1E

; TwelveBran is a table of multiples of $0C, used as a branch table when setting NudgeVal
TwelveBran: .byte   $00, $0C, $18, $24, $30, $3C, $48, $54

; modes we are using and their index values above.  xC = branch table value
; 0 xC = 00 - 40 char Apple III color               text    nomix   lores
; 1 xC = 0C - Fg/bg hires (280x192, 16 colors)      text    nomix   hires
; 2 xC = 18 - super hires (560x192, b/w)            gr      mix     hires
; 3 xC = 24 - 140x192 A Hires (140x192, color)      text    mix     hires   scroll

; Screen layout:
; mode 2 (bw hires)  lines 00-0F (10)        b/w map display
; mode 0 (text)      lines 10-1F (10) 02-03  score/status
; mode 3 (a3 hires)  lines 20-3F (20)        hires map upper field  map: 00-1F
; mode 0 (text)      lines 40-8F (50) 08-11  text play field        map: 20-27
; mode 3 (a3 hires)  lines 90-AF (20)        hires map lower field  map: 28-47
; mode 1 (a3 medres) lines B0-BF (10) 15-17  medium res something

; a 6502 interrupt takes minimum 7 cycles from IRQ to the first instruction
; of the handler executing.  The current instruction finishes first, and if
; it is a long one (like a 6 cycle RTS or something), if might be 13 cycles
; after the interrupt before we start executing here.
; The blanking intervals are not going to wait around for us, though.
; I can reset the timer in 27, putting it 34-40 cycles after the interrupt.
; That's in time not to miss any HBLs, so it should be able to count to 8.
; Next interrupt should come around at 65 cycles after the first.
; Ideally the mode switch should happen then. Takes 22 cycles to switch
; to modes other than 3, which just barely fits in the HBL space (25).
; mode 3 has a smooth scroll setting as well, also takes 22 cycles to set.
; If I'm switching to mode 3, can try doing smooth scroll first.
; Then for any mode maybe poll for HBL (burns a couple of cycles for
; mode 3, and around 25 for modes < 3) and then switch in the second one.
; polling would take 7 cycles to loop, 6 to succeed.
; One advantage of polling is that it can buy me one more sync point with
; MAME, since it would burn the 2MHz clock cycles in the second line.
; However, I can't get polling to work on real hardware.  ?
;
; Resetting the HBL timer is considered to be top priority.  This happens within
; 27 cycles, so no HBLs should be missed before the timer is reset.
; This means that the timer gets reset just as the drawing resumes on the next
; line, and it is another 38 cycles before we enter the next HBL.
; However, we must exit the interrupt handler before 520 cycles have elapsed, or
; we will miss the timer we set for 8 lines hence.  (65 cycles x 8)
; 
; HBL is the longest one, because it handles both graphics mode switching and
; audio. 

; the HBL fires, and we have 65 cycles at 1MHz to reset the timer, or 130 cycles at 2MHz.
; to avoid tearing, mode switch could be before 25 or between 65 and 80 (1MHz)
;   or before 50, between 130 and 160 (2MHz if running in a too-fast MAME environment)
; having reset the timer, then we try to fit the mode switch into the HBL after trigger.
; we manage: timer 30 (60)
; the HBL fires, and we have 25 cycles before the next line starts drawing.
; since that's unreasonably soon, wait instead to do switch between 65 and 80 cycles
; (Which is within the next HBL after the one that triggered it)

; This code embeds some of the global game variables within it, so the interrupt code
; never looks up the next mode, or nudge value, because the game just stores it inline.
; to speed up both mode and nudge processing, the values are multiples of $0C, used
; as a branch offset.  The TwelveBran table above can be used to look up those multiples.
; If you are storing into NudgeVal (held directly in the interrupt code), use the smooth
; scroll parameter to look up the multiple of $0C in TwelveBran.

; keyboard interrupt handler - just pass it on to the event loop
; 30 (or 27) cycles in here, 21 to get here - 51 (or 48) total
intkey:     lda IO_KEY          ;4 load keyboard register
            bpl keyreturn       ;2/3 no key pressed, return (could that even happen? modifier only?)
            sta KeyCaught       ;4 tell event loop to process this
keyreturn:  bit IO_KEYCLEAR     ;4 [14 or 11] clear keyboard register
            lda #$01            ;2 clear the keyboard (CA2) interrupt
            sta RE_INTFLAG      ;4 [20 or 17]
            pla                 ;4
            rti                 ;6 [30 or 27]

; timer1 (clock during VBL) interrupt handler
; 29 in here (if clearing timer); 17+intaudio in here (if not), 37 to get here.  Total 66, 54+intaudio
intclock:   lda #$02            ;2 clear the timer1 interrupt
            sta RE_INTFLAG      ;4
            dec ClockTick       ;6 countdown number of interrupts we are doing
            bmi tickdone        ;2/3 if we are done all we expect during VBL, stop the clock interrupts
jmpaudio:   jmp intaudio        ;3 [17] go do the sound
tickdone:   sta RE_INTENAB      ;4 [19] disable clock interrupt (we're done) by writing $02 here
            pla                 ;4
            rti                 ;6 [29]

; check for interrupts other than HBL after we have established that it is not HBL
; 12 to get here.
nothbl:     lda RE_INTFLAG      ;4 check for other interrupts
            and #$01            ;2 [18] was it the keyboard?
            bne intkey          ;2/3 if yes, go handle it [after 21]
            lda RE_INTFLAG      ;4
            and #$10            ;2 [26] was it VBL?
            bne intvbljmp       ;2/3 if yes, go handle it [after 29, 32 with the jmp]
            lda RE_INTFLAG      ;4
            and #$02            ;2 [34] was it the clock?
            bne intclock        ;2/3 if so, go handle it [after 37]
            pla                 ;4
            rti                 ;6 [46 if we did not recognize an interrupt]
intvbljmp:  jmp intvbl          ;3

; main interrupt handler entry point - first thing is to check for HBL and handle it
; non-HBL branches to up above.

inthandle:  pha                 ;3 stash A because we need it
            lda RE_INTFLAG      ;4 identify the interrupt we got
            and #$20            ;2 [9] is it HBL after all?
            beq nothbl          ;2/3 branch+jump off to the rest of the interrupt handlers if not
            ; this is the HBL interrupt
            sta RE_INTFLAG      ;4 [now 15] clear the HBL interrupt (A is still $20)
            lda #$07            ;2 reset the timer2 flag for 8 HBLs from now
            sta RE_T2CL         ;4
            lda #0              ;2 and...
            sta RE_T2CH         ;4 go! [27 to here, next line started drawing, 38 to go before next HBL]
            ; do smooth scroll first while we wait for beam to travel horizontally
NudgeVal    =   nudgebran + 1   ; smooth scroll parameter x $0C - directly modifies the interrupt handler code
nudgebran:  beq nudge1          ;3 [30] then 15 cycles, 12 bytes per block
nudge0:     bit SS_XXN
            bit SS_XNX
            bit SS_NXX
            jmp postnudge
nudge1:     bit SS_XXY
            bit SS_XNX
            bit SS_NXX
            jmp postnudge
nudge2:     bit SS_XXN
            bit SS_XYX
            bit SS_NXX
            jmp postnudge
nudge3:     bit SS_XXY
            bit SS_XYX
            bit SS_NXX
            jmp postnudge
nudge4:     bit SS_XXN
            bit SS_XNX
            bit SS_YXX
            jmp postnudge
nudge5:     bit SS_XXY
            bit SS_XNX
            bit SS_YXX
            jmp postnudge
nudge6:     bit SS_XXN
            bit SS_XYX
            bit SS_YXX
            jmp postnudge       ; at this point (each block but last: 45, last: 42)
nudge7:     bit SS_XXY
            bit SS_XYX
            bit SS_YXX
            ; polling below doesn't work on real hardware, seems to work fine in MAME.
            ; though also does not seem to matter whether I wait for bit set or bit clear.
postnudge:  ;bit R_TONEHBL       ; 4 burn cycles until HBL arrives (expecting 20, about 3 loops)
            ;bvc postnudge       ; 2/3
            sec
NextMode    =   modebran + 1    ; screen mode to switch into next - directly modifies the interrupt handler code
modebran:   bcs ismode1         ;3 into blank
ismode0:    sta D_TEXT          ;mode 0 - +00 - 40 char A3 text [19 cycles]
            sta D_NOMIX
            sta D_LORES
            sta D_SCROLLOFF
            jmp isddone
ismode1:    sta D_TEXT          ;mode 1 - +0F - medres [19 cycles]
            sta D_NOMIX
            sta D_HIRES
            sta D_SCROLLOFF
            jmp isddone
ismode2:    sta D_GRAPHICS      ;mode 2 - +1E - super hires [19 cycles]
            sta D_MIX
            sta D_HIRES
            sta D_SCROLLOFF
            jmp isddone
ismode3:    sta D_TEXT          ;mode 3 - +2D - A3 hires [46* cycles]
            sta D_MIX           ;^4 4
            sta D_HIRES         ;4
            sta D_SCROLLON      ;4 [16 for this block, 19 for the others, so 19/22 into second blank]
isddone:    stx IntXStash       ;4 [53 for modes < 3, 68 for mode 3] stash X
ScrRegion   =   nextregion + 1  ; upcoming region, counts down from $17 - modifies the interrupt handler code
nextregion: ldx #$17            ;2 [55 for modes < 3, 70 for mode 3] prepare for next mode switch
            dex                 ;2
            bpl snextmode       ;2/3 we MUST reset this if somehow we roll off the bottom
            ldx #$16            ;2 [61, 76 depending on mode] this is the topmost region
snextmode:  stx ScrRegion       ;4 [64, 79 depending on mode -- or 65, 80 if rolled]
            lda ScrRegMode, x   ;4*
            sta NextMode        ;4 [72*, 87* -- 73*, 88*]
            jmp intaudiob       ;3 [75*, 90* -- 76*, 91*] go do the audio, we already stashed X
            ; +65 HBL went by for sure, +130 HBL coming up

; VBL interrupt handler
; screen timing: 65 1MHz cycles per scan line, 192 lines, should be painting for 12480 cycles.
; then 70 lines' worth of VBL.  Would be 4550 cycles at 1MHz, but we are running at 2MHz, so ~9000
; To keep the audio running, we set a clock timer to fire 8 times during VBL, at $208 (520) cycles.
; real hardware couldn't handle it, switching to 4 at $410 cycles.

; 80 cycles in here, 44 to get here, 124 total.
;  Skipping 12 (92, 136) right now

; Tweaked the T2 timer to get it to look good both in MAME and on real hardware.
; By pushing the trigger down later, $05 in T2 works on both. If it is higher,
; $06 looks good in MAME and $05 looks good on real hardware (because MAME goes faster).

intvbl:     lda #$06            ;2 reset the HBL counter for top region when it eventually comes
            sta RE_T2CL         ;4
            lda #$0             ;2
            sta RE_T2CH         ;4 [70 skipping RTC part]
            sta D_GRAPHICS      ;4 assume that the screen starts (after VBL)
            sta D_MIX           ;4 in mode 2 (Apple III bw hires)
            sta D_HIRES         ;4
            sta D_SCROLLOFF     ;4 smooth scrolling off is assumed for top region
            lda #$10            ;2 clear the VBL (CB1) interrupt
            sta RE_INTFLAG      ;4
            lda #$16            ;2 [30]
            sta ScrRegion       ;4 reset next region number to $16 (counts down from $17)
SRegFstNxt  =   vblfstnext + 1  ; the first "next" region, set up by setupenv.
vblfstnext: lda #$00            ;2 the first "next" region
            sta NextMode        ;4 [40]
            dec VBLTick         ;6 bump VBL countdown
            dec VBLTickP        ;6 bump VBL countdown
            lda #$03            ;2 fire the clock interrupt 4 times during VBL
            sta ClockTick       ;4 [58]
            lda #$10            ;2
            sta RE_T1CL         ;4 [64] interval is $410, this is the $10 part.
            ;lda #%10000010      ; 2 enable CA1 (RTC)
            ;sta RE_INTENAB      ; 4
            ;lda #$04            ; 2
            ;sta RE_T1CH         ; 4 [68 to here] start the clock for $208 cycles
            ;lda #$0             ;2
            ;sta RE_T2CH         ;4 [70 skipping RTC part]
            pla                 ;4
            rti                 ;6

; timer2 (HBL) interrupt handler
; the graphics mode switching stuff was all handled in the primary interrupt handler.
; what is left here is the audio processing (also used by the clock timer during VBL)
; sound data is in bank 1, and it is set up to constantly play a background, which can
; be interrupted by sound effects.  Has provisions for multiple background sounds,
; sequenced using BackNext (the high byte of the address in bank 1 where next one starts).
; unless something changes it, it will just keep cycling back to the one at $2000.
; this switches into ZP $1A00 to allow for extended addressing.

intaudio:   stx IntXStash       ;4 stash X if we didn't already do that in HBL
intaudiob:  sty IntYStash       ;4 stash Y because we will use it - cycle counts start from here
            lda R_ZP            ;4 stash ZP because we will use it
            pha                 ;3
            ;jmp timerout        ; debug skip audio
            lda #$1A            ;2 move ZP to $1A00 (so extended addressing works and for access to pointers)
            sta R_ZP            ;4
            ldy #$00            ;2 [19]
PlaySFX     =   toggsfx + 1     ; play sound effects switch (user controlled)
toggsfx:    lda #$00            ;2 [21] check sfx switch (user controlled)
            beq doback          ;2/3 if no sound effects should be played, skip to background [after 24]
FXPlaying   =   runsfx + 1      ; nonzero if a sound effect is playing
runsfx:     lda #$00            ;2 [25] see if a sound effect is playing
            beq doback          ;2/3 if no sound effect is playing, down to background [after 28]
            lda (ZFXPtr), y     ;5* [32*] load next sfx sample
            bpl playfx          ;2/3 if we have not hit the end of the sample go play it [after 35*]
            sty FXPlaying       ;4 stop playing sfx
            jmp doback          ;3 do background sound instead [after 41*]
playfx:     sta R_TONEHBL       ;4 [39*] send out the sfx sample
            inc ZFXPtr          ;5 [44*] move to the next sample
            bne backnext        ;2/3 if not over page break skip ahead [after 47*]
            inc ZFXPtr + 1      ;4
            jmp backnext        ;3 [53*] go advance the background sound even though it did not play
            ; cycle counting
            ; soundtrack playing but ends gets us here after +12*
BackNext    =   backdone + 1    ; page number of first sample of next background segment after this one
backdone:   lda #$00            ; 2 move to next background sound segment
            sta ZSoundPtr + 1   ; 3 put sample start address into the pointer
            sty ZSoundPtr       ; 3
            ; sfx shut off by user gets us here after 24
            ; no sfx playing gets us here after 28
            ; sfx playing but ends gets us here after 41*
            ; if soundtrack playing but ends, add +20* cycles to get back here
PlaySound   =   doback + 1      ; play soundtrack switch (user controlled)
doback:     ldx #$00            ;2 check sound switch (user controlled)
            beq backnext        ;2/3 if background sound should not play, skip play (but still advance)
            lda (ZSoundPtr), y  ;5* load next background sample
            bmi backdone        ;2/3 if we hit the end of the sample block, move to next sample block
            sta R_TONEHBL       ; 4
            ; sfx plays, gets us here after 47* or 53* if pointer rolls over
            ; via doback, soundtrack shut off by user gets us here after +5
            ; via doback, soundtrack playing gets us here after +15*
backnext:   inc ZSoundPtr       ; 5 move to the next background sound sample
            bne timerout        ; 2/3 branch if the byte didn't roll over
            inc ZSoundPtr + 1   ; 5 update high byte of the sample pointer
            ; above adds 8 if no rollover, 12 if rollover.
            ; cycle count by condition.  Add four if we got here via HBL.  Conceivable there are errors.
            ; if sfx is playing, 55* (61* if sfx pointer rolls, 59* if strk pointer rolls, 65* if both do)
            ; sfx off 24 - strk off 5 - 8 = 37 or 41 if strk pointer rolls
            ; sfx off 24 - strk plays 15* - 8 = 47* or 51* if strk pointer rolls
            ; sfx off 24 - strk skips 35** - 8 = 67**
            ; sfx none 28 - strk off 5 - 8 = 41 or 45 if strk pointer rolls
            ; sfx none 28 - strk plays 15* - 8 = 51* or 55* if strk pointer rolls
            ; sfx none 28 - strk skips 35** - 8 = 71**
            ; sfx ends 41* - strk off 5 - 8 = 54* or 58* if strk pointer rolls
            ; sfx ends 41* - strk plays 15* - 8 = 64** or 68** if strk pointer rolls
            ; sfx ends 41* - strk skips 35** - 8 = 84***
            ; so range of possible cycle counts is 37 (both off) to 84*** (sfx ends, strk skips)
timerout:   ldx IntXStash       ;4 [26 cycles to get out from here]
            ldy IntYStash       ;4
            pla                 ;4
            sta R_ZP            ;4
            pla                 ;4
            rti                 ;6
            ; total possible cycle counts is 63 (both off) to 110*** (sfx ends, strk skips)
            ; if coming from HBL, that is after 75*, 90* -- 76*, 91*
            ; so best case 138*, worst case 201****

; arm interrupts and setup interrupt handler environment

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
            lda ScrRegModB      ; first next mode
            sta NextMode
            sta SRegFstNxt      ; put it in the interrupt handler VBL reset code as well
            lda #$16
            sta ScrRegion       ; first next region is $16
            lda #$01
            sta PlaySFX         ; start by assuming we will play sound effects
            sta PlaySound       ; start by assuming we will play background sound
            lda #$00
            sta FXPlaying       ; no sound effect currently playing
            sta ZSoundPtr
            sta ZFXPtr
            lda #$20            ; first background segment starts at $2000 in bank 1
            sta ZSoundPtr + 1   ; segment we are currently playing
            sta BackNext        ; segment we will play next after this one
            lda #$81            ; sound information in bank 1
            sta ZSoundPtr + XByte
            sta ZFXPtr + XByte

            ; bank register - $FFEF - E-VIA input register A
            ; ZP register - $FFD0 - D-VIA input/output register B
            
            ; set environment - $FFDF - D-VIA input register A
            ; because we will use ZP to draw, we need stack to be true most of the time
            ;     0------- 2MHz clock           (1=1MHz)
            ;     -1------ C000.CFFF I/O        (0=RAM)
            ;     --1----- video enabled        (0=disabled)
            ;     ---1---- Reset key enabled    (0=disabled)
            ;     ----0--- C000.CFFF read/write (1=read only)
            ;     -----1-- True stack ($100)    (0=alt stack)
            ;     ------1- ROM#1                (0=ROM#2)
            ;     -------1 F000.FFFF RAM        (1=ROM)
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
            ; high nibble here is largely irrelevant, ineterrupts CB1, CB2 not used
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
            ; PB6 appears to be (at least?) HBL.
            ; E-VIA: enable timer 2, one-shot.  HBL.
            ; was this
            ;lda #%00100000
            ;     00100000 - timer2 count down pulses on PB6 (HBL)
            ;     01100000 - timer1 continuous (restart when hits zero)
            lda #%11100000 ; enable timer1 and timer2
            sta RE_AUXCTL
            
            ; E-VIA - CA2 is keyboard, CA1 is clock; CB1, CB2 are VBL
            ; CB2 011- hi nibble - independent interrupt input pos edge (VBL)
            ; CB1 ---0 hi nibble - neg active edge (VBL)
            ; CA2     001- lo nibble independent interrupt input neg edge (keyboard)
            ; CA1     ---0 lo nibble neg active edge (clock)
            lda #%01100010
            ;lda #%00100010 ; neg active edge?  CB2 may be VBLx8.
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
