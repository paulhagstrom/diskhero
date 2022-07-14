; Diskhero
; Interrupt-related routines
;
; setupenv - set and arm the interrupts
; interrupt handlers for VBL, HVL, keyboard
;
; The SOS manual prescription is for interrupts to use the true ZP
; But since we're taking over the machine we do not have to honor that
; We do want to set it to something deterministic, but if we set it to $1A00 (interpreter ZP)
; then the interpreter can communicate with the interrupt handler more easily, and
; indirect addressing becomes available in the interrupt handler.
; So as long as I can avoid running out of ZP space, I will defy convention and use $1A00.

IntAStash:  .byte 0
IntZStash:  .byte 0

NudgePos:   .byte   0   ; smooth scroll parameter A (used when ScrNudge for a region is positive)
NudgeNeg:   .byte   0   ; smooth scroll parameter B (used when ScrNudge for a region is negative)
VBLTick:    .byte   0   ; ticked down for each VBL, can use to delay things for several refreshes

; see diskhero.inc for ZP definitions (generally trying to stay between D0-FF)

; The following three groups define the screen regions
; ScrRegMode is the graphics mode of the region as defined by setdisplay
; ScrNudge indicates whether smooth scroll is set to inactive (0), A (pos) or B (neg)
; ScrRegDur is the height of the region in groups of 8-line blocks.  Text rows, essentially.
; They are copied into ZScrRegMod, ZScrRegDur, ZScrNudge and used there
ScrRegMode: .byte   $01, $06, $07, $01, $07, $03, $05, $00
ScrNudge:   .byte   $00, $00, $01, $00, $01, $00, $80, $00
ScrRegDur:  .byte   $02, $02, $04, $09, $04, $01, $02, $00      ; groups of 8

            ; VBL: 21 + 2 + 15 + 22 + intvbl = 60 + intvbl
            ; Key: 21 + 3 + 8 + 15 + 22 + intkey = 69 + intkey
            ; HBL: 21 + 3 + 9 + 9 + inttimer + 22 = 64 + inttimer

; keyboard interrupt handler - just pass it on to the event loop
; 54 cycles in here, 47 to get here - 101 total
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

; VBL interrupt handler
; should correspond to a bump to 2MHz for the duration of the blank
; at 1MHz a full paint should take 65*192 cycles (12480)
; and then 70 lines' worth during VBL corresponding to 4550 cycles at 1MHz.
; And if we are really running at 2MHz during VBL, we should have 9100 cycles available.
; MAME may be overestimating the cycles we have available during non-VBL since it may be
; running at 2MHz even then.  To stay compatible with real HW, must be conservative with cycles.
; during paint we have a regular DAC interrupt with HBL (520 cycles, every 8 lines).
; during VBL, we should try to get the extra 8.75 DAC feeds in.
; the screen must be painting at 60Hz rather than 30Hz.
; if I do 32.75 events in a thing going 60Hz, I should be able to make a 1965 Hz sound,
; or, since I have to oscillate, a 982.5 Hz sound.  In the neighborhood of A440.
; but the timer during VBL should be firing at about 1965 Hz, or lie .51 milliseconds.
; can I set a RTC-based interrupt during VBL for that?
; 
; 83 cycles (reliably) in here, 55 to get here, 138 total.
intvbl:     lda #$06            ; 2 reset the HBL counter for top region
            sta RE_T2CL         ; 4
            lda #$0             ; 2
            sta RE_T2CH         ; 4
            sta D_TEXT          ; 4 assume that the screen starts (after VBL)
            sta D_NOMIX         ; 4 in mode 1 (Apple III color text)
            sta D_LORES         ; 4
            sta D_SCROLLOFF     ; 4 smooth scrolling off is assumed for top region
            lda #$01            ; 2 
            sta ZScrRegion      ; 3 reset next region number to 1
            lda ZScrRegDur      ; 3
            sta ZScrRegBand     ; 3
            dec VBLTick         ; 4 bump VBL countdown
            lda #$10            ; 2 clear the VBL (CB1) interrupt
            sta RE_INTFLAG      ; 4 [49 up to here]
            pla                 ; 4 [34 cycles to end, restoring environment]
            tax                 ; 2
            pla                 ; 4
            tay                 ; 2
            lda IntZStash       ; 4
            sta R_ZP            ; 4
            lda IntAStash       ; 4
            rti                 ; 6

; keyboard and VBL handlers are above here just to ensure they can be reached by
; branches

; We explicitly set the ZP to 1A here so we can talk to the interpreter.

inthandle:  sta IntAStash       ; 4
            lda R_ZP            ; 4
            sta IntZStash       ; 4
            lda #$1A            ; 2
            sta R_ZP            ; 4
            tya                 ; 2
            pha                 ; 3
            txa                 ; 2
            pha                 ; 3
            cld                 ; 2 [30 to here, stashing environment]
            lda RE_INTFLAG      ; 4 identify the interrupt we got
            and #$20            ; 2 was it the timer/HBL (most pressing one)
            bne inttimer        ; 2/3 if yes, go handle it [after 24 here]
            lda RE_INTFLAG      ; 4
            and #$01            ; 2 was it the keyboard?
            bne intkey          ; 2/3 if yes, go handle it [after 31 here]
                                ; kbd handler is another 34-40ish, so 64-71ish cycles total
            lda RE_INTFLAG      ; 4
            and #$10            ; 2 was it VBL?
            bne intvbl          ; 2/3 if yes, go handle it [after 39 here]
                                ; vbl handler is another 58, so 87 cycles total
intreturn:  pla                 ; 4 [34 cycles to end, restoring environment]
            tax                 ; 2
            pla                 ; 4
            tay                 ; 2
            lda IntZStash       ; 4
            sta R_ZP            ; 4
            lda IntAStash       ; 4
            rti                 ; 6 [?? if no interrupt recognized]

; timer2 (HBL) interrupt handler
; sets the display mode and smooth scroll offset, then timer for next mode switch point
; allows for two different smooth scroll offsets, named "NudgePos" and "NudgeNeg"
; set to go off only every 8 lines
; timing: each scan line takes 65 1MHz cycles, 40 of which are video, 25 of which are HBL.
; when this fires, we have 65 cycles before we start missing HBL interrupts
; it burns 42 just getting here (and needs 22 after), so we will miss one for sure.
; we will miss two if we take more than 66 cycles to reset the timer and return out.
; if we are doing just DAC we can make it out in time not to miss the second one.
; if we are also switching video modes at the end of a band, it can go up to something like 167.
; so we miss one at 66, and another at 131, and another at 196.
; HOWEVER: MAME seems to run too fast during non-HBL, so will not miss as many.  May need a MAME switch.
; i.e. real hw: wait for 7 (no audio, mid-band), 6 (audio, mid-band), or 5 (region switch)
; MAME: wait for 7 (mid-band) or 6 (region switch)?
; mid-band no sample: 11 + 23 = 34
; mid-band sample end: 11 + 22 + 7 + 13 = 53
; mid-band sample play: 11 + 22 + 6 + 11 = 50
; end band: 10 + 12 + setdisplay + (10)/(13+setnudge)/(15+setnudge) + 14 + sample-above
;  or 36 + sample-stuff-minus-1 + setdisplay + 10/13+setnudge/15+setnudge
;  setdisplay and setnudge each average around 32.
;  so say 36 + 52 + 32 + 15 + 32 is the most full.  167.  Missed two at 66, another at 131.
;  in most cases, we are going to miss three HBLs.   Might even want to dawdle on the short path.
;  we would miss the third at 196, so we can maybe spare another 30ish cycles.
; tl;dr: when we reset the timer, we tell it to wait for 6 because we missed two.

; if I am going to do sound I should try to do it right.
; so I should use bank 1 for sound data.
; sound effects will be in the lower pages (0F, 0E, 0D)
; I have from 0000 to 7FFF.  So, I'll use 8K chunks for three background variants.
; $2000-a, $4000-b, $6000-c, and then sound effects will be in pages down from $2000 ($1F etc).
; have a background flag indicating which background is playing, a sequencer for ordering abc that loops
; maybe try to keep the background ticking when the effects are running.
; try making samples that are 6-bit 1KHz?
; got here after 24 cycles in interrupt handler.
; trust that ZSoundPtr will survive between interrupts and points somewhere readl
;
; takes 39 cycles to get in here, 34 cycles at the end to get out.
; so just getting in and out and doing nothing else misses one HBL (minimum 73, 8 cycles after 65)
;
inttimer:   ldy #$00            ; 2
            lda ZFXPlay         ; 3 [18] see if a sound effect is playing
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
doback:     lda (ZSoundPtr), y  ; 5* load next background sample
            bmi backdone        ; 2/3 if we hit the end of the sample, move to next one
            sta R_TONEHBL       ; 4
backnext:   inc ZSoundPtr       ; 5 move to the next background sound sample
            bne doregion        ; 2/3
            inc ZSoundPtr + 1   ; 5
doregion:   ldy #$07            ; 2 HBLs we expect before next 8th line if just DAC
            dec ZScrRegBand     ; 5 bump countdown of bands in this region
            bne justdac         ; 2/3 if the region is still being drawn, skip ahead to timer reset
            ldx ZScrRegion      ; 3 put upcoming region (that has now arrived) into X
            lda ZScrRegMode, x  ; 4 move display to correct mode
            ror                 ;2 this is just setdisplay (32) but copied in to save 12 cycles
            bcs isdtext         ;2/3
            bit D_GRAPHICS      ;4
            ror                 ;2
            bcs isdmix          ;2/3
isdnomix:   bit D_NOMIX         ;4
            ror                 ;2
            bcs isdhires        ;2/3
isdlores:   bit D_LORES         ;4
            jmp isddone         ;3
isdtext:    bit D_TEXT          ;4
            ror                 ;2 
            bcc isdnomix        ;2/3
isdmix:     bit D_MIX           ;4
            ror                 ;2
            bcc isdlores        ;2/3
isdhires:   bit D_HIRES         ;4
isddone:    lda ZScrNudge, x    ; 4 nudge if this region needs nudging
            beq nonudge         ; 2/3 do no nudging
            bmi negnudge        ; 2/3 using negative (alt) nudge?
            lda NudgePos        ; 4 nope, use the positive (regular) nudge value
            jmp gonudge         ; 3
nonudge:    and D_SCROLLOFF     ; 4 turn smooth scroll off
            jmp postnudge       ; 3
negnudge:   lda NudgeNeg        ; 4 yep, use the negative (alternate) nudge value
gonudge:    ror                 ; this is just setnudge (32) but copied in to save 12 cycles
            bcs isnxxy
            bit SS_XXN
            ror
            bcs isnxyx
isnxnx:     bit SS_XNX
            ror
            bcs isnyxx
isnnxx:     bit SS_NXX
            jmp isndone
isnxxy:     bit SS_XXY
            ror
            bcc isnxnx     
isnxyx:     bit SS_XYX
            ror
            bcc isnnxx
isnyxx:     bit SS_YXX
isndone:    lda D_SCROLLON      ; 4 turn smooth scroll on (nudge)
postnudge:  lda ZScrRegDur, x   ; 4 reset the HBL counter to length of next region
            sta ZScrRegBand     ; 3
            inc ZScrRegion      ; 5 point ScrRegion to the upcoming one
            ldy #$06            ; 2 wait 8 lines for next interrupt (missed one already)
justdac:    sty RE_T2CL         ; 4
            lda #0              ; 2 clear the timer2 flag
            sta RE_T2CH         ; 4
            lda #$20            ; 2 clear the HBL interrupt
            sta RE_INTFLAG      ; 4
            pla                 ; 4 [34 cycles to end, restoring environment]
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
            ; initialize zero page data that interrupt handlers rely on
            ldy #$07            ; 8 screen bands
:           lda ScrRegMode, y
            sta ZScrRegMode, y
            lda ScrRegDur, y
            sta ZScrRegDur, y
            lda ScrNudge, y
            sta ZScrNudge, y
            dey
            bpl :-
            lda #$01
            sta ZScrRegion
            lda ZScrRegDur
            sta ZScrRegBand
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
