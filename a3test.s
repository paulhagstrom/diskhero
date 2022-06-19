; Initially borrowed from robj's softcard.s
;
; Can use AppleCommander to write this as a raw binary starting at $A200
; called SOS.INTERP (or maybe whatever.INTERP).
;
; Memory details: $2000-9FFF are the bankable areas. The rest is always there.
; SOS lives in the upper region ($A000-FFFF)
; Highest bank in a 128K system is 2, 6 for 256K, E for 512K
; FFEF is the bank register.
; Enhanced indirect addressing uses the X-byte.  Hi bit is "enhanced", low nibble is bank.
; put 89 32 at 57.  Put 82 at 1658.  Ldy 0, lda (57),y.  You get 82:3289.
; X-byte is 1600 above the hi byte of the address you're indirecting with.
; This mode uses all 64K, puts target bank in 0000-7FFF, next one (if) in 8000-FFFF.
; you should not try to reach 8n:0000-00FF or 8n:FF00-FFFF, use an adjacent bank
; X-byte 8F is special: puts bank 0 in 2000-9FFF, S-bank everywhere else.
; SOS and interp have separate ZPs (1800 and 1A00) and stacks (0100, 1B00)
; Interps want to be placed as high as possible, with the last bit of it poking into
; the S-bank, so in principle doesn't get switched out.  Though they don't want to
; guarantee this in future versions of SOS.  But A000-B800 is below SOS and in S-bank.
; you can request memory with REQUEST_SEG (specifies area) or FIND_SEG (looks for area).

; the VIA situation is complex, so I will take some notes here as I decode them
; from the Apple III Service Reference Manual 1982
; the two VIAs are general purpose chips that have defined behaviors outside the
; Apple III context, and these behaviors have specialized purposes within the AIII context.
; D-VIA is accessed with FFDX. E-VIA is accessed with FFEX.
; FFX4, FFX5 timer 1 counter L, H. FFX6, FFX7 - timer 1 latch L, H
; Reading Counter L will reset the T1 interrupt, reading Latch L will not
; Writing to Latches just writes to latches.
; Writing to Counter L will write to Latch L but won't change counter.
; Writing to Counter H will write to Latch H, send Latches to Counters, and reset T1 int
; Timer 1 behavior controlled in FFXB (writeo!) auxiliary control register
; bit 6: 1 = timed interrupt each time T1 is loaded, 2 = continuous interrupts
; bit 7: 0 = PB7 disabled, 1 = one-shot output (timed) or square wave output (continuous)
; Timer 2 behavior is bit 5: 0 = timed, 1 = count down with pulses on PB6
; bit 1 PB, bit 0 PA: 1 = enable latching, 0 = disable latching
; bits 4, 3, 2 = shift register control
; FFX8, FFX9 timer 2 counters. There do not seem to be timer 2 latches.
; FFXC - is the peripheral control register.  High nibble CB, low nibble CA.
; LSB determines whether interrupt is neg active edge (0) or pos active edge (1)
; higher bits:
; 000 input negative active edge
; 001 independent interrupt input neg edge
; 010 input positive active edge
; 011 independent interrupt input pos edge
; 100 handshake output
; 101 pulse output
; 110 low output
; 111 high output
; FFX0 - input/output register "B" (IRB/ORB)
; FFD2 - data direction for ? either XRA or XRB, unclear
; FFXA - shift register
; FFXB - auxiliary control register, shift register mode control
; 000 = disabled, 100 = shift out free running at t2 rate
; 0xx = shift in, 1xx = shift out
; under control of x01 T2, x10 |2?, x11 ext clk
; FFXE - Interrupt enable register, FFXD - Interrupt flag register
; enable write: bit 7 determines whether we are setting (1) or clearing (0)
; enable read: bit 7 is 0, other reflect current enabled state.
; flag: bit 7 is set by any enabled interrupt, cleared by clearing all
; bit 6: timer 1 (T1 times out, clear by reading T1 L or writing T1 H)
; bit 5: timer 2 (T2 times out, clear by reading T2 L or writing T2 H)
; bit 4: CB1 (CB1 active edge, clear by r/w ORB unless CB1 is independent)
; bit 3: CB2 (CB2 active edge, clear by r/w ORB unless CB2 is independent)
; bit 2: Shift register (8 shifts complete, clear by r/w shift reg)
; bit 1: CA1 (CA1 active edge, clear by r/w ORA unless CA1 is independent)
; bit 0: CA2 (CA2 active edge, clear by r/w ORA unless CA2 is independent)

; E-VIA port A handles bank switching (PA0-PA2),
; slot 1 (PA4), slot 2 (PA5), closed apple (PA6).  Any IRQ anywhere triggers PA7.
; E-VIA port B handles sound (PB0-PB5), I/O Count (PB6), and slot NMI (PB7)
; E-VIA CA2 is keyboard interrupt, CB1, CB2, and Shift are for VBL.
; CA1 - RTC's clock IRQ, negative edge active input.
; CA2 - keyboard interrupt, independent negative edge interrupt, sets bit 0 of IFR.
; CB1, CB2, Shift - vertical blanking
; for each VBL: make CB2 independent, or "let it strobe the IPB" and set the bit flag
; for every 8 VBLs: use Shift to count to 8.
; The description (p69) of PB6 is: PB6 is connected to the I/O Count line.
; Depending on the device in the slots, the VIA may be programmed to count a certain
; number of pulses generated or to determine that only one pulse occurred.  Either way,
; the VIA will generate an IRQ and set the appropriate bit flag.
; Atomic Defense appears to be using PB6 to sense *horizontal* blanking, but it is
; not yet clear to me how we know that it will work for this.  Maybe insider info.

; D-VIA port A handles environmental register
; PA0-PA1 - ROM behavior. F000-FFFF. PA0: 0-RAM, 1-ROM; PA1: 0-ROM2, 1-ROM1
; is there ANY way to use ROM2 on existing Apples III?
; PA2 - stack. 0 = alt, 1 = true (0100)
; PA3 - RAM rw disable. 0 = rw, 1 = ro
; PA4 - see Reset key and slot NMIs. 0 = disabled (no Reset), 1 = enabled
; PA5 - "used to modify the blanking signal" video 0 = disabled, 1 = enabled
; PA6-PA7 not mentioned, but 6 is C0-CF 0=RAM, 1=I/O; 7 is clock 0=2MHz, 1=1MHz
; CA1 is connected with slot IRQs - if a slot requests an interrupt, this toggles.
; CA2 is "Sw1/Mgnsw" - can be set to cause an interrupt on either edge.
;   It could be used to have a Function Only run while the switch is depressed, or v-v.
; SCO/SER connected to CB1/CB2 - elementary serial data port,
;   usually strobe to a serial R0 printer.  Could also be configured as serial input
;   or just interrupts for two external lines.
; PB2 - zero page register AND address register for RTC.  If you use it for RTC, you
;   need to restore the ZPR afterwards.

; Handling VBL:
; E-VIA is in charge of this, CB1 and CB2 and Shift (if you want to count to 8).
; to set VBL interrupts:
; read FFEE (interrupt enable register) so we leave other interrupts alone
; OR with $88 (CB2) or $90 (CB1) or $84 (shift) ?
; write FFEE (keep old interrupts and enable new ones)
; to choose edge?
; read FFEC (peripheral control register) to leave everything else intact
; OR with god knows what.
; maybe $00 (input neg active edge interrupt neg active edge)
; or $05 (input pos active edge interrupt pos active edge)
; perhaps I can get hints from looking at code.
; Atomic Defense interp writes $20 (indep inp neg edge interr neg active edge CB2)
; clears the keyboard strobe, does SEI, and jumps to gamecode.
; Atomic Defense font loader reads, clears bit 6 and 7, sets bit 5, and writes.
; Seems overly complicated. Could have just read, AND #$0F, ORA #$20, write, no?
; Preserves bit 4 (potentially interrupt pos active edge?)
; font loader then stores $08 in E_IFR (CB2 interrupt bit) which clears it,
; pauses, then waits for it to trip.
; Atomic Defense intro reads E_PCR, ORA with $18, writes. Appears to be:
; VBL: pos active edge (CB1), Keyboard: handhake output (CA2), Clock: neg act edge (CA1) 
; Atomic Defense game: writes $00 into D_ACR and E_ACR.
; writes $62 into E_PCR
;   CB2: inp pos active edge - CB1: neg act edge (VBL)
;   CA2: ind interr inp neg edge (keyboard) - CA1: neg act edge (clock)
; writes $76 into D_PCR
;   CB2: ind interr inp pos edge - CB1: pos act edge (probably joystick)
;   CA2: inp pos active edge (sw1) - CA1: neg act edge (slot irq)
; writes $7F into D_IER and D_IFR (disables and clears all interrupts)
; writes $0E into E_IER and E_IFR
;   disable and clear shift, CB2, CA1
; writes $F1 into E_IER
;   set timer 1, timer 2, CB1, CA2
; writes $71 into E_IFR
;   clear timer 1, timer 2, CB1, CA2
; Atomic Defense interrupt handler treats VBL as start of drawing.

            .segment "CODE"
            .setcpu "6502"

KBD         = $C000     ; keyboard
KBDFLAG     = $C008     ; keyboard flag
KBDCLEAR    = $C010     ; keyboard strobe

BEEP        = $C040     ; beep
SPEAKER     = $C030     ; click
GRAPHICS    = $C050     ; "clear text mode"
TEXT        = $C051     ; set text mode
NOMIX       = $C052     ; "clear mix mode"
MIX         = $C053     ; "set mix mode"
PAGEONE     = $C054     ; "clear PG2 mode"
PAGETWO     = $C055     ; PG2 mode
LORES       = $C056     ; "clear hires mode"
HIRES       = $C057     ; set hires mode
TEXTBUFA    = $0400     ; line 0
TEXTBUFB    = $0480     ; line 1
TEXTBUFC    = $0500     ; line 2
TEXTBUFD    = $0580     ; line 3
TEXTBUFE    = $0600     ; line 4
TEXTBUFF    = $0680     ; line 5
GRBUFA      = $2000     ; start of page 1

SCROLLON    = $C0D9     ; enable smooth scroll (vertical shift)
SCROLLOFF   = $C0D8     ; disable smooth scroll
LC000       = $C000     ; used as base for slow scroll offset
LC002       = $C002
LC004       = $C004

E_IORB      = $FFE0            ; includes HBL?
    ; maybe this is undocumented?  Atomic defense seems to use
    ; bit 6 for horizontal blanking detections, tested with bvc.
    ; supposed to be "input register B."  Bit 6 is listed as IOCT.
    ; Spelled out "I/O Count" in the PB port description.
    ; PB0-PB5: inputs to the sound generator.  PB6 is I/O count.
    ; PB7 is NMI monitor for slots.
    
D_ACR       = $FFDB            ; ffdx auxiliary control register
E_ACR       = $FFEB            ; ffex auxiliary control register
D_PCR       = $FFDC            ; ffdx peripheral control register
E_PCR       = $FFEC            ; ffex peripheral control register
D_IFR       = $FFDD            ; ffdx interrupt flag register
E_IFR       = $FFED            ; ffex interrupt flag register
    ; bit 0=kbd, 1=CA1, 2=shift, 3=CB2 (vblx8), 4=CB1 (vblx1), 5=timer 2, 6=timer 1, 7=IRQ
D_IER       = $FFDE            ; ffdx interrupt enable register
E_IER       = $FFEE            ; ffex interrupt enable register
; In III's Company Programming Q&A it was said
; that writing 01 here will disable the keyboard interrupt, 81 re-enables.
; that looks like the reverse of what I wrote above for E_IFR.  Is bit 0 the MSB?

EReg        = $FFDF            ; Environment register
; 0 F000.FFFF 0 = RAM 1 = ROM
; 1 ROM# 0 = ROM#2, 1 = ROM#1
; 2 stack 0 = alt, 1 = true (0100)
; 3 C000.CFFF 0 = rw, 1 = ro
; 4 reset key 0 = disabled, 1 = enabled
; 5 video 0 = disabled, 1 = enabled
; 6 C000.CFFF 0 = RAM, 1 = I/O
; 7 clock 0 = 2MHz, 1 = 1MHz
Bank_Reg    = $FFEF             ; Bank register
; low nibble - current bank, high = ?
IRQVECT     = $ffcd             ; Monitor IRQ points here
Z_REG       = $ffd0             ;zero page register

;SOS Calls
;REQUEST_SEG    = $40
;RELEASE_SEG    = $45
TERMINATE   = $65
;OPEN        = $C8
;READ        = $CA
;CLOSE        = $CC

; indirect addressing
Ptr1        = $20
;Ptr2        = $22
CExtPG      = $1601          ; Interp extended address offset
Numbor      = $22

; modes from a3 level 2 service reference manual
; Apple /// memory map claims:
;    text mode    C050 - clear / C051 set ("bw/color")
;    mix mode    C052 - clear / C053 set ("lores/hires")
;    PG2 mode    C054 - clear / C055 set
;    Hires mode    C056 - clear / C057 set ("text/graphics")
; 40 char A2. P1: 0400-07FF, P2: 0800-0BFF
;    lores, nomix, graphics(!) - display mode 0
; 40 char A3. P1: char 0400-07FF color 0800-0BFF. P2: reverse
;    color: bits 4-7 FG, 0-3 BG.
;    lores, nomix, text - display mode 1
; 80 char B&W. P1: primary 0400-07FF secondary 0800-0BFF. P2: reverse
;    primary: char 1, 3, 5, etc.; secondary: char 2, 4, 6, etc.
;    lores, mix, graphics(!) - display mode 2 or 3?
; B&W hires: 280x192. P1: 2000-3FFF, P2: 4000-5FFF.
;    hires, nomix, graphics - display mode 4
; medres 16 color. 280x192. pixels: 2000-3FFF, color: 4000-5FFF.
;    each 7-bit block: LSB....MSB. (MSB is ignored)
;    hires, nomix, text(!) - display mode 5
; SHR B&W. 560x192. P1: primary 2000-3FFF secondary 4000-5FFFF.
;    P2: primary 6000-7FFF, secondary 8000-9FFF.
;    page flipping is possible!
;    MSB is ignored. pixels are LSB...MSB.
;    primary: odd dot groups, secondary: even dot groups
;    hires, mix, graphics - display mode 6
; A3 Hires. 140x192, 16 colors per pixel. 4 bits per pixel.
;    2000 and 4000, fetches alternate.  LSB 12345678 MSB
;    pixel 1: 2000 1234
;    pixel 2: 2000 567 4000 1
;    pixel 3: 4000 2345
;    pixel 4: 4000 67 2001 12
;    pixel 5: 2001 3456
;    pixel 6: 2001 7 4001 123
;    pixel 7: 4001 4567
;    and so on.
;     P1: primary 2000, secondary 4000; P2: primary 6000, secondary 8000
;    page flipping is possible!
;    hires, mix, text(!) - display mode 7
; other things of interest:
;    FFF3 - Hires bank switch ?
;    FFF5 - display modes ?
;    FFEX - CB1 - vertical blanking, CB2 - vertical blanking

; What I would like to try is
; Do a VBL interrupt, then switch modes on the descent.
; then move pixels during the VBL, which, due to the ///'s architecture
; should give us a fair amount of time for pixel movement.
; will take some artistry to get this working right.
; can also use character sets for the text modes.

; $2000 is the standard load address, and this should be in the
; highest bank, so minimally bank 2, possibly bank 6, depending on 128/256K.

            .org     $2000 - 14
            
; sos interp header
            .byte    "SOS NTRP"
            .word    0000
            .word    CodeStart
            .word    (CodeEnd-CodeStart)

CodeStart:  jmp Init

; put the interrupt handler here at the beginning so that we're
; unlikely to get any 6502 page-crossing penalties

HandleInt:  pha
            tya
            pha
            txa
            pha
            cld
            lda E_IFR
            and #$10        ; CB1 (VBL)
            beq :+          ; not VBL
            jsr DoVBL
            lda #$10        ; clear CB1 VBL
            sta E_IFR
            bne IntReturn
:           lda E_IFR
            and #$01        ; CA2 (Keyboard)
            beq :+          ; not keyboard
            jsr DoKeyboard 
            lda #$01        ; clear CA2 keyboard
            sta E_IFR
            bne IntReturn
:           lda E_IFR
            and #$20        ; timer 2
            bne DoTimer
IntReturn:  pla
            tax
            pla
            tay
            pla
            rti
; timer2 interrupt handler
DoTimer:    lda #$20        ;clear timer2 flag
            sta E_IFR
            ; do game tasks
            ; IntReturn is probably too far away to branch to, so replicate
            pla
            tax
            pla
            tay
            pla
            rti
; keyboard interrupt handler
DoKeyboard: lda KBD
            sta KBDCLEAR
            bpl KeyReturn   ; no key pressed, return
            ; buffer keys
KeyReturn:  rts
; VBL interrupt handler
DoVBL:      lda TEXT        ; display mode 1 (A3 40 text) (status)
            lda NOMIX
            lda LORES
            ldx #24         ; stay in mode 1 for 24 lines
:           bit E_IORB
            bvc :-          ; wait until bit 6 (BL) is set
            dex
            beq :++
:           bit E_IORB
            bvs :-          ; wait until bit 6 (BL) is clear again
            bvc :--         ; wait for the next line
:           lda GRAPHICS    ; display mode 6 (A3 bw superhires) (map)
            lda HIRES
            lda MIX
            ldx #24         ; stay in mode 6 for 24 lines
:           bit E_IORB
            bvc :-          ; wait until bit 6 (BL) is set
            dex
            beq :++
:           bit E_IORB
            bvs :-          ; wait until bit 6 (BL) is clear again
            bvc :--         ; wait for the next line
:           lda TEXT        ; display mode 7 (A3 hires) (side view)
            ; set nudge
            ; turn nudging on
            ldx #48         ; stay in mode 7 for 48 lines
:           bit E_IORB
            bvc :-          ; wait until bit 6 (BL) is set
            dex
            beq :++
:           bit E_IORB
            bvs :-          ; wait until bit 6 (BL) is clear again
            bvc :--         ; wait for the next line
:           lda NOMIX       ; display mode 5 (A3 medres) (sky)
            ; turn nudging off
            ; set nudge
            ; turn nudging on
            ldx #48         ; stay in mode 5 for 48 lines
:           bit E_IORB
            bvc :-          ; wait until bit 6 (BL) is set
            dex
            beq :++
:           bit E_IORB
            bvs :-          ; wait until bit 6 (BL) is clear again
            bvc :--         ; wait for the next line
:                           ; no mode switch but maybe nudge switch
            ldx #48         ; stay in mode 5 for 48 lines
:           bit E_IORB
            bvc :-          ; wait until bit 6 (BL) is set
            dex
            beq :++
:           bit E_IORB
            bvs :-          ; wait until bit 6 (BL) is clear again
            bvc :--         ; wait for the next line
:                           ; should have run off the bottom now
            lda TEXT        ; return to mode 1 (A3 40 text) (status)
            lda LORES
            rts

            .include "font.s" ; UploadFont

varA:       .byte   $C1
linecount:  .byte   0
nudgecount: .byte   0
antinudge:  .byte   0
paintfrom:  .byte   0
IRQSave:    .res    0, 3

; start of game
Init:       
            lda IRQVECT
            sta IRQSave
            lda IRQVECT + 1
            sta IRQSave + 1
            lda IRQVECT + 2
            sta IRQSave + 2
            lda #$77        ; 2MHz, video, I/O, reset, r/w, ram, ROM#1
            sta EReg
            jsr UploadFont
            ; show intro screen
            ; initialize graphics
            ; set up VIAs
            ; setup vars and interrupt
            lda #$0C
            sta Z_REG       ; put zeropage at page $0C
            lda EReg
            and #$FB        ; enable alternate stack
            sta EReg
            ; rts to task 0
            
demo:       ldx #$00        ;display mode
            jsr dispmodex
            lda PAGEONE
            lda SCROLLOFF
            lda KBDCLEAR
            jsr DrawText
            lda #$00
            sta paintfrom
            jsr paint

            lda #$31        ;enable IOCT and keyboard
            sta E_IER

scrollgo:   lda #$07
            sta nudgecount
            lda #$00
            sta antinudge
scrollpos:  lda #$10        ; clear VBLx1 flag
            sta E_IFR
:           lda E_IFR
            and #$10        ; VBL x1?
            beq :-
            ; wait for 24 scanlines to pass
            ; and switch to mode 4
            ldx #24
            jsr waithbl
            ldx #$04
            jsr dispmodex
            lda SCROLLON
            ; wait for 24 scanlines to pass
            ; and switch to mode 5
            ldx #24
            jsr waithbl
            ldx #$05
            jsr dispmodex
            ; wait for 24 scanlines to pass
            ; and switch to mode 1
            ldx #24
            jsr waithbl
            ldx #$01
            jsr dispmodex
            lda SCROLLOFF
            ldx antinudge
            jsr nudgetox

            ; wait for 24 scanlines to pass
            ; and switch to mode 7
            ldx #24
            jsr waithbl
            ldx #$07
            jsr dispmodex
            lda SCROLLON

            ; wait for 24 to pass and switch back to mode 0
            ldx #24
            jsr waithbl
            ldx #$00
            jsr dispmodex
            lda SCROLLOFF
            
;            lda KBD
;            bmi gotkey

            jsr DrawNum

            ; nudge screen during VBL
            ldx nudgecount
            jsr nudgetox
            
            inc antinudge
            dec nudgecount
            bmi sevdone
            
            jmp scrollpos
            
sevdone:    ;inc        varA
            ;jsr        fillx
            ;jsr        paint

            jmp scrollgo
gotkey:     lda SCROLLOFF

            lda #$7f       ;disable all via interrupts
            sta D_IER
            sta D_IFR
            lda #$7f
            sta E_IER
            sta E_IFR

            brk
            .byte   TERMINATE
            .word   *-2

; wait for x (x-register as param) lines to be drawn
waithbl:    bit E_IORB
            bvc waithbl
            ; wait for HBL to go low again
waithblb:   bit E_IORB
            bvs waithblb
            dex
            bne waithbl
waitedhbl:  rts

; write to the text page

DrawText:   lda #$8F
            sta Ptr1+CExtPG
            lda #$04
            sta Ptr1 + 1
            lda #$00
            sta Ptr1
            lda #$A0
            ldy #$27
:           sta (Ptr1),y
            dey
            bpl :-
            inc Ptr1 + 1
            ldy #$27
:           sta (Ptr1),y
            dey
            bpl :-
            lda #$80
            sta Ptr1
            dec Ptr1 + 1
            ldy #$00
:           lda StatText,y
            beq :+
            ora #$80
            sta (Ptr1),y
            iny
            bne :-
:           lda #$A0
            sta (Ptr1),y
            iny
            cpy #$28
            bne :-
            
            lda #$A8
            sta Ptr1
            lda #$05
            sta Ptr1 + 1
            ldy #$00
:           lda ColorText, y
            beq :+
            sta (Ptr1), y
            iny
            bne :-
:           lda #$09
            sta Ptr1 + 1
            ldy #$0F
:           tya
            sta (Ptr1), y
            dey
            bpl :-
            lda #$B8
            sta Ptr1
            ldy #$0F
:           tya
            asl
            asl
            asl
            asl
            sta (Ptr1), y
            dey
            bpl :-
            
            ; FONTCHAR is defined in font.s
DrawNum:    lda #$90
            sta Ptr1
            lda #$04
            sta Ptr1+1
            lda #$8F
            sta Ptr1+CExtPG
            lda #$00
            sta Numbor
            ldy #$05
:           lda ClickNum, y ; base number to display
            tax
            lda FONTCHAR, x ; character this corresponds to
            clc
            adc RollNum, y
            ora Numbor
            sta (Ptr1), y
            lda #$80
            sta Numbor
            dey
            bpl :-
            
IncNum:     ldx #$05
:           inc RollNum,x
            lda RollNum,x
            cmp #$08
            bne IncNumDone
            lda #$00
            sta RollNum,x
            inc ClickNum,x
            lda ClickNum,x
            cmp #$0A
            bne IncNumDone
            lda #$00
            sta ClickNum,x
            dex
            bpl :-
IncNumDone: rts

StatText:   .byte "NUM BER #! ?:"
            .byte $0

ColorText:  .byte "ABCDEFGHIJKLMNOP"
            .byte "ABCDEFGHIJKLMNOP"
            .byte $0

ClickNum:   .byte $0,$0,$0,$0,$0,$0
RollNum:    .byte $0,$0,$0,$0,$0,$0

; HGR graphics is at the beginning of bank 0
paint:      lda #$00
            sta Ptr1+CExtPG
            lda #$00
            sta Ptr1
            sta Ptr1+1
            lda paintfrom
            ldx #$10
            ldy #$00
paintpage:  sta (Ptr1), y
            iny
            bne paintpage
            clc
            adc #$01
            dex
            bne paintpage
            rts

; set display to number in X (clobbers A, Y)

dispmodex:  ldy DMAMAP,x
            lda LC000,y
            ldy DMBMAP,x
            lda LC000,y
            ldy DMCMAP,x
            lda LC000,y
            rts

; set nudge value to X (clobbers A, Y)

nudgetox:   ldy VAIMAP,x
            lda LC000,y
            ldy VBIMAP,x
            lda LC002,y
            ldy VCIMAP,x
            lda LC004,y
            rts
            
; mapping tables to convert X to binary VAI, VBI, VCI values
; borrowed from Atomic Defense

VAIMAP:     .byte    $e0,$e1,$e0,$e1,$e0,$e1,$e0,$e1
VBIMAP:     .byte    $e0,$e1,$e1,$e0,$e0,$e1,$e1,$e0
VCIMAP:     .byte    $e0,$e1,$e1,$e1,$e1,$e0,$e0,$e0

; mapping table for display modes
; 0 = 40 char Apple II b/w
; 1 = 40 char Apple III color
; 2 = 80 char b/w
; 3 = 80 char b/w
; 4 = Apple II hires (280x192 b/w)
; 5 = Fg/bg hires (280x192, 16 colors)
; 6 = super hires (560x192, b/w)
; 7 = 140x192 A Hires (140x192, color)
DMAMAP:     .byte    $56,$56,$56,$56,$57,$57,$57,$57
DMBMAP:     .byte    $52,$52,$53,$53,$52,$52,$53,$53
DMCMAP:     .byte    $50,$51,$50,$51,$50,$51,$50,$51

CodeEnd     = *