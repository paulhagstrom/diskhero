; Diskhero
; Interrupt-related routines
;
; setupenv - set and arm the interrupts
; interrupt handlers for VBL, HVL, keyboard

PlayingSnd: .byte   0, 0        ; store address of sample here
SampleInd:  .byte   0           ; if nonzero, a sample is played until it becomes zero
NudgePos:   .byte   0
NudgeNeg:   .byte   0
ScrRegion:  .byte   0
ScrRegBand: .byte   0
ScrRegMode: .byte   $01, $06, $07, $01, $07, $03, $05, $00
ScrNudge:   .byte   $00, $00, $01, $00, $01, $00, $80, $00
ScrRegDur:  .byte   $02, $02, $04, $09, $04, $01, $02, $00      ; groups of 8
VBLTick:    .byte   0

inthandle:  pha                 ; save registers
            tya
            pha
            txa
            pha
            cld
            lda RE_INTFLAG      ; identify the interrupt we got
            and #$10            ; was it VBL?
            beq :+              ; nope, check the next one
            jsr intvbl          ; yep, handle the VBL interrupt
            lda #$10            ; clear the VBL (CB1) interrupt
            sta RE_INTFLAG
            bne intreturn       ; branch always
:           lda RE_INTFLAG
            and #$01            ; was it the keyboard?
            beq :+              ; nope, check the next one
            jsr intkey          ; yep, handle the keyboard interrupt
            lda #$01            ; clear the keyboard (CA2) interrupt
            sta RE_INTFLAG
            bne intreturn       ; branch always
:           lda RE_INTFLAG
            and #$20            ; was it the timer/HBL?
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
; update: trying to fire it every 8 so I can control the DAC from here too.
inttimer:   ldx #$07            ; HBLs we expect before next 8th line
            dec ScrRegBand      ; bump countdown of bands in this region
            bne justdac         ; if the region is still being drawn, skip ahead to sound
            ldx ScrRegion       ; put upcoming region (that has now arrived) into X
            lda ScrRegMode, x   ; move display to correct mode
            jsr setdisplay
            lda ScrNudge, x     ; nudge if this region needs nudging
            beq nonudge         ; do no nudging
            bmi negnudge        ; using negative (alt) nudge?
            lda NudgePos        ; nope, use the positive (regular) nudge value
            jmp gonudge
nonudge:    and D_SCROLLOFF     ; turn smooth scroll off
            beq postnudge       ; branch always
negnudge:   lda NudgeNeg        ; yep, use the negative (alternate) nudge value
gonudge:    jsr setnudge        ; twiddle the nudge bits
            lda D_SCROLLON      ; turn smooth scroll on (nudge)
postnudge:  lda ScrRegDur, x    ; reset the HBL counter to length of next region
            sta ScrRegBand
            inc ScrRegion       ; point ScRegion to the upcoming one
            ldx #$06            ; wait 8 lines for next interrupt (missed one already)
justdac:    stx RE_T2CL
            lda #0              ; clear the timer2 flag
            sta RE_T2CH
            lda #$20
            sta RE_INTFLAG
            ldy SampleInd       ; get sample index
            beq intreturn       ; nothing to do on DAC, we are done
            lda Samples, y
            bmi stopdac         ; if we hit end of sample marker (byte over $7F), stop playing
            sta R_TONEHBL
            dec SampleInd
            jmp intreturn
stopdac:    lda #$00
            sta SampleInd
            beq intreturn

; keyboard interrupt handler
intkey:     lda IO_KEY          ; load keyboard register
            bpl keyreturn       ; no key pressed, return (could that even happen? modifier only?)
            sta $0400           ; put it in the corner so I can see it
            sta KeyCaught       ; tell event loop to process this
keyreturn:  bit IO_KEYCLEAR     ; clear keyboard register
            rts

; VBL interrupt handler
intvbl:     lda #$06            ; reset the HBL counter for top region
            sta RE_T2CL
            lda #0
            sta RE_T2CH
            lda ScrRegMode      ; move display to correct mode
            jsr setdisplay
            lda D_SCROLLOFF     ; smooth scrolling off is assumed for top region
            lda #$01
            sta ScrRegion       ; reset next region number to 1
            lda ScrRegDur
            sta ScrRegBand
            dec VBLTick         ; bump VBL countdown
vblrts:     rts

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
