; Diskhero
; Interrupt-related routines
;
; Against convention, the interrupt handlers will use $1A00 as the ZP for two reasons.
; One is so the interpreter (also using ZP $1A00) can set parameters used (quickly) by
; the interrupt handler, and two is so that interrupt handler processing audio has the
; ability to use extended addressing to grab the next audio sample.

; see diskhero.inc for ZP definitions (generally trying to stay between D0-FF)

IntAStash:  .byte   0   ; saved A register from interrupt handler entry
IntXStash:  .byte   0   ; saved X register from interrupt handler entry
IntZStash:  .byte   0   ; saved ZP register from interrupt handler entry

; variables used in processing the screen splitting
ScrRegion:  .byte   0   ; upcoming region (the one after the one we are in) counts downward from $17
NudgePos:   .byte   0   ; smooth scroll parameter - MUST BE BETWEEN 0 AND 7 when HBL interrupt fires
NextMode:   .byte   0   ; screen mode to switch into next - MUST BE VALID when HBL interrupt fires

; variables for sound and speed management
VBLTick:    .byte   0   ; ticked down for each VBL, can use to delay things for several refreshes
VBLTickP:   .byte   0   ; playfield ticker, try to draw during VBL
ClockTick:  .byte   0   ; ticked down for each clock-during-VBL, for playing sound (only) during VBL


; Screen regions: (screen splitting definition)
; ScrRegMode is the graphics mode of the region as defined in the table below
;   encoded as a multiple of $0C so that it can be used as a branch/jump table.
; This is set up to split in blocks of 8 scan lines, to do multiples, repeat the mode
; Last mode (only) will use the smooth scroll parameter.
; These are in reverse order so that I can quickly detect if it runs off the end somehow
; and reset it to the top.  ScrRegModB is used in resetting, it is "NextMode" when resetting.
ScrRegMode: .byte   $0C, $0C, $24, $24, $24, $24, $00, $00
            .byte   $00, $00, $00, $00, $00, $00, $00, $00
            .byte   $24, $24, $24, $24, $00, $00 
ScrRegModB: .byte   $18, $18

; TwelveBran is a jump table used for setting smooth scroll, 8 multiples of $0C
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

; Getting the HBL timer reset is urgent enough that I will do that even before
; properly stashing the environment.  To dodge an inaccuracy with MAME (which at
; present does not downshift to 1MHz during video drawing) I need to get this timer
; reset within the first 32 cycles ideally (MAME running at double speed, and 65
; cycles before first HBL would be missed).  Next most crucial thing is switching
; video modes fast because real hardware is already progressing down the screen.

nothbljmp:  jmp nothbl          ; 19 once departing from here.  Jump to rest of interrupt handlers.

inthandle:  sta IntAStash       ; 4 save A
            clc                 ; 2 assume by default that this is not HBL (signal to later code)
            lda RE_INTFLAG      ; 4 identify the interrupt we got
            and #$20            ; 2 is it HBL after all?
            beq nothbljmp       ; 2/3 branch+jump off to the rest of the interrupt handlers
            sta RE_INTFLAG      ; 4 clear the HBL interrupt
            lda #$07            ; 2 reset the timer2 flag for 8 HBLs from now
            sta RE_T2CL         ; 4
            lda #0              ; 2 and...
            sta RE_T2CH         ; 4 go! [30 to get to this point, made it before any HBLs even on MAME]
            stx IntXStash       ; 4
            lda NextMode        ; 4
            sta modebranch + 1  ; 4 modify next instruction to go to the right place
modebranch: bne ismode1         ; 3 [45 to here]
ismode0:    bit D_TEXT          ;mode 0 - +00 - 40 char A3 text [15 cycles]
            bit D_NOMIX
            bit D_LORES
            jmp isddone
ismode1:    bit D_TEXT          ;mode 1 - +18 - medres [15 cycles]
            bit D_NOMIX
            bit D_HIRES
            jmp isddone
ismode2:    bit D_GRAPHICS      ;mode 2 - +24 - super hires [15 cycles]
            bit D_MIX
            bit D_HIRES
            jmp isddone
ismode3:    bit D_TEXT          ;mode 3 - +30 - A3 hires (3 cycles fewer to switch to mode 7)
            bit D_MIX           ; ^4 4
            bit D_HIRES         ; 4 also will use smooth scroll
            lda D_SCROLLON      ; 4 turn smooth scroll on (nudge)
            ldx NudgePos        ; 4 get smooth scroll value - CRITICAL THAT THIS BE BETWEEN 0 AND 7
            lda TwelveBran, x   ; 4* find proper branch value
            sta nudgebran + 1   ; 4 update branch to go to the right place
nudgebran:  bne nudge1          ; 3 [35 in block to here], then 15 cycles, 12 bytes per block
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
            jmp postnudge
nudge7:     bit SS_XXY
            bit SS_XYX
            bit SS_YXX
            jmp postnudge       ; at this point (each block) 50 cycles in mode 4, for 107 total
isddone:    and D_SCROLLOFF     ; 4 [76 for modes other than 4] turn smooth scroll off
postnudge:  ldx ScrRegion       ; 4 prepare for next mode switch
            dex                 ; 2
            bpl snextmode       ; 2/3 we MUST reset this if somehow we roll off the bottom
            ldx #$16            ; 2 this is the topmost region
snextmode:  stx ScrRegion       ; 4
            lda ScrRegMode, x   ; 4*
            sta NextMode        ; 4            
            ldx IntXStash       ; 4 restore X so it can pushed onto the stack properly when stack arrives
            sec                 ; 2 [now 44 if we jumped here] remember that we took this path, to reach rest of handler
            ; cycle count status update: (out of date)
            ;  19 - not HBL, 44 - HBL but mid-region so no switch needed
            ;  95 - HBL switching to mode other than 4 (65 HBL went by)
            ; 130 - HBL switching to mode 4 and setting smooth scroll (65 HBL went by, 130 HBL maybe?)
nothbl:     lda R_ZP            ; 4 ok, now properly stash the environment
            sta IntZStash       ; 4
            lda #$1A            ; 2
            sta R_ZP            ; 4
            tya                 ; 2
            pha                 ; 3
            txa                 ; 2
            pha                 ; 3
            cld                 ; 2 [26 to here since update]
            bcs jmpaudio        ; 2/3 if we got here via the HBL timer, go do sound processing
            lda RE_INTFLAG      ; 4 check for other interrupts
            and #$01            ; 2 was it the keyboard?
            bne intkey          ; 2/3 if yes, go handle it [after 11+26 + 19 = 56 to here]
                                ; kbd handler is another 34-40ish, so 64-71ish cycles total
            lda RE_INTFLAG      ; 4
            and #$10            ; 2 was it VBL?
            bne intvbl          ; 2/3 if yes, go handle it [after 19+26 + 19 = 64 to here]
                                ; vbl handler is another 58, so 87 cycles total
            lda RE_INTFLAG      ; 4
            and #$02            ; 2 was it the clock?
            bne intclock        ; 2/3 if so, go handle it
intreturn:  pla                 ; 4 [34 cycles to end, restoring environment]
            tax                 ; 2
            pla                 ; 4
            tay                 ; 2
            lda IntZStash       ; 4
            sta R_ZP            ; 4
            lda IntAStash       ; 4
            rti                 ; 6

; keyboard interrupt handler - just pass it on to the event loop
; 54 cycles in here, 56 to get here - 100 total
intkey:     lda IO_KEY          ; 4 load keyboard register
            bpl keyreturn       ; 2/3 no key pressed, return (could that even happen? modifier only?)
            sta KeyCaught       ; 4 tell event loop to process this
keyreturn:  bit IO_KEYCLEAR     ; 4 clear keyboard register
            lda #$01            ; 2 clear the keyboard (CA2) interrupt
            sta RE_INTFLAG      ; 4
            pla                 ; 4 [34 cycles to end, restoring environment]
            tax                 ; 2
            pla                 ; 4
            tay                 ; 2
            lda IntZStash       ; 4
            sta R_ZP            ; 4
            lda IntAStash       ; 4
            rti                 ; 6

; timer1 (clock during VBL) interrupt handler
intclock:   lda #$02            ; 2 clear the timer1 interrupt
            sta RE_INTFLAG      ; 4
            dec ClockTick       ; 4 countdown number of interrupts we are doing
            bmi tickdone        ; 2/3 if we are done all we expect during VBL, stop the clock interrupts
jmpaudio:   jmp intaudio        ; 3 go do the sound
tickdone:   sta RE_INTENAB      ; disable clock interrupt (we're done) by writing $02 here
            jmp intreturn       ; and out

; VBL interrupt handler
; screen timing: 65 1MHz cycles per scan line, 192 lines, should be painting for 12480 cycles.
; then 70 lines' worth of VBL.  Would be 4550 cycles at 1MHz, but we are running at 2MHz, so ~9000
; To keep the audio running, we set a clock timer to fire 8 times during VBL, at $208 (520) cycles.
; real hardware couldn't handle it, switching to 4 at $410 cycles.

; 98 cycles in here, 64 to get here, 162 total.
; Tweaked the T2 timer to get it to look good both in MAME and on real hardware.
; By pushing the trigger down later, $05 in T2 works on both. If it is higher,
; $06 looks good in MAME and $05 looks good on real hardware (because MAME goes faster).

intvbl:     lda #$05            ; 2 reset the HBL counter for top region when it eventually comes
            sta RE_T2CL         ; 4
            sta D_GRAPHICS      ; 4 assume that the screen starts (after VBL)
            sta D_MIX           ; 4 in mode 2 (Apple III bw hires)
            sta D_HIRES         ; 4
            sta D_SCROLLOFF     ; 4 smooth scrolling off is assumed for top region
            lda #$10            ; 2 clear the VBL (CB1) interrupt
            sta RE_INTFLAG      ; 4
            lda #$16            ; 2
            sta ScrRegion       ; 4 reset next region number to $16 (counts down from $17)
            lda ScrRegModB      ; 4 the first "next" region
            sta NextMode        ; 4
            dec VBLTick         ; 6 bump VBL countdown
            dec VBLTickP        ; 6 bump VBL countdown
            lda #$03            ; 2 fire the clock interrupt 4 times during VBL
            sta ClockTick       ; 4
            lda #$10            ; 
            sta RE_T1CL         ; 4 interval is $410, this is the $10 part.
            ;lda #%10000010      ; 2 enable CA1 (RTC)
            ;sta RE_INTENAB      ; 4
            ;lda #$04            ; 2
            ;sta RE_T1CH         ; 4 [68 to here] start the clock for $208 cycles
            lda #$0             ; 2
            sta RE_T2CH         ; 4
            pla                 ; 4 [30 cycles to end, restoring environment]
            tax                 ; 2
            pla                 ; 4
            tay                 ; 2
            lda IntZStash       ; 4
            sta R_ZP            ; 4
            lda IntAStash       ; 4
            rti                 ; 6

; timer2 (HBL) interrupt handler
; the graphics mode switching stuff was all handled in the primary interrupt handler.
; what is left here is the audio processing (also used by the clock timer during VBL)
; sound data is in bank 1, and it is set up to constantly play a background, which can
; be interrupted by sound effects.  Has provisions for multiple background sounds,
; sequenced using ZBackNext (the high byte of the address in bank 1 where next one starts).
; unless something changes it, it will just keep cycling back to the one at $2000.

intaudio:   ;jmp timerout
            ldy #$00            ; 2
            lda ZPlaySFX        ; 3 check sfx switch (user controlled)
            beq doback          ; 2/3 if no sound effects should be played, skip to background
            lda ZFXPlay         ; 3 see if a sound effect is playing
            beq doback          ; 2/3 if no sound effect is playing, down to background
            lda (ZFXPtr), y     ; 5* load next sfx sample
            bpl playfx          ; 2/3 if we have not hit the end of the sample go play it
            sty ZFXPlay         ; 3 stop playing sfx
            jmp doback          ; 3 do background sound instead
playfx:     sta R_TONEHBL       ; 4 send out the sfx sample
            inc ZFXPtr          ; 5 move to the next sample
            bne backnext        ; 2/3
            inc ZFXPtr + 1      ; 4
            jmp backnext        ; 3 go advance the background sound even though it did not play
backdone:   lda ZBackNext       ; 4 move to next background sound segment
            sta ZSoundPtr + 1   ; 3 put sample start address into the pointer
            sty ZSoundPtr       ; 3
doback:     ldx ZPlaySound      ; 3 check sound switch (user controlled)
            beq backnext        ; 2/3 if background sound should not play, skip play (but still advance)
            lda (ZSoundPtr), y  ; 5* load next background sample
            bmi backdone        ; 2/3 if we hit the end of the sample block, move to next sample block
            sta R_TONEHBL       ; 4
backnext:   inc ZSoundPtr       ; 5 move to the next background sound sample
            bne timerout        ; 2/3
            inc ZSoundPtr + 1   ; 5
timerout:   pla                 ; 4 [34 cycles to end, restoring environment]
            tax                 ; 2
            pla                 ; 4
            tay                 ; 2
            lda IntZStash       ; 4
            sta R_ZP            ; 4
            lda IntAStash       ; 4
            rti                 ; 6

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
            lda #$16
            sta ScrRegion       ; first next region is $16
            lda #$01
            sta ZPlaySFX        ; start by assuming we will play sound effects
            sta ZPlaySound      ; start by assuming we will play background sound
            lda #$00
            sta ZFXPlay
            sta ZSoundPtr
            sta ZFXPtr
            lda #$20            ; first background segment starts at $2000 in bank 1
            sta ZSoundPtr + 1   ; segment we are currently playing
            sta ZBackNext       ; segment we will play next after this one
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
