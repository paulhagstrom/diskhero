; DISKHERO
; Apple III Hires region
; map display upper and lower block

; paint the whole map (updates afterwards are relative, leveraging smooth scroll)

initmap:    lda #$20            ; top field starts at display line $20
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
            cmp #$60            ; did we just fall off the map into the bottom void?
            bne novoid          ; no, so continue on
:           inc CurScrLine      ; we are in the bottom void, advance the graphics raster line
            ldx CurScrLine      ; note that if we are in the lower void, we must be in bottom field
            cpx #$A9            ; last line of bottom field complete? ($88-$A8)
            beq imdone
            jsr drawvoid
            jmp :-
novoid:     inc CurScrLine      ; advance the graphics raster line
            ldx CurScrLine
            cpx #$A9            ; last line of bottom field complete? ($88-$A8)
            beq imdone          ; if so, move to the next screen region
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
imdone:     rts
            

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
; top field starts at $20, draws $20 lines of map (offsets 0-1F),
; middle play field starts at 40, draws 7 lines of map (offsets 20-26),
; bottom field starts at $88, draws $20 lines of map (offsets 27-46).
; The zero point (no void, map offsets from 0-46 are rendered) has HeroY at 23.
; So, the top visible line is HeroY - 23,
; the last visible line in the upper field is HeroY - 4
; playfield goes from HeroY - 3 to HeroY + 3
; and bottom field goes from HeroY + 4 to HeroY + 23.
;
; Suppose HeroY advances from 23 to 24.  The map scrolls up one.  Nudge was 0,
; we will set nudge to 1, and it starts drawing line 1 (second line) first.
; Which is great, because the top visible line should be HeroY - 23,
; and the data is already there.  But we need line 0 (first line) to hold
; the graphics for the line below 7.  Line 0 of the next group down has that data.
; So we move line 8 up to line 0, line 10 to 8, line 18 to 10, but then we need
; to provide a new line 18.  That's going to come from HeroY - 4.  Now: 20.
; That matches what is written above, where "oldnudge" was zero.
; For the lower field, we draw line HeroY + 23 (47) into graphics line A0 (88+18)
;
; Suppose HeroY decreases from 24 to 23. The map scrolls down one.
; top visible line should be HeroY - 23, so map line 0.
; Map line 0 wasn't there anymore, because it had line 8 in it.  We need to replace it.
; So we move line 10 to line 18, line 8 to 10, line 0 to 8
; and then draw line 0 into 0.  As written above, where "newnudge" is 0.
; For the lower field, we draw line HeroY + 23 (46) into graphics line 88 (88+0).
;
; Suppose HeroY is 26.  That is 3 down from the zero point.
; That means the map has scrolled up 3 steps, so nudge starts off at 3.
; Each group of 8 lines are drawing in this sequence: 3 4 5 6 7 0 1 2.
; The top visible line represents 
; Assuming the screen is rendered properly ahead of time, now we are scrolling.
; when we are increasing nudge from 3 to 4 (map is moving up, drawing starts lower),
; we will want to replace line 3 with the line that should be rendered below 2.
; 
; When everything's done, nudge will be ( HeroY + 5 ) mod 8.  (+5 is the same as -3)
; Inc/dec of HeroY determines which line needs to be drawn anew.  Nudge here refers to
; what it ends up as.
; HeroY increased to 24, nudge goes to 1, raster line $17 + nudge + $20 ($38) gets map HeroY - 4 ($20).
; must copy line $7 + $20 + nudge ($28) to -1 + $20 + nudge ($20), up to $17 + nudge + $20 ($38) to $10.
; lower field raster line $17 + nudge + $88 ($A0) gets map line HeroY + $23 ($47)
; HeroY decreased to 23, nudge goes to 0, raster line $0 + nudge + $20 ($20) gets map line HeroY - $23 (0).
; lower field raster line $0 + nudge + $88 ($88) gets map line HeroY + 4 ($27)

; enter with carry clear to increase nudge (with old NudgePos),
; or carry set to decrease nudge (with new already-decreased NudgePos).
; in other words, nudge should be the lowest of new and old
; HeroY just got incremented (carry clear) or decremented (carry set) prior to calling this
updatemap:  bcs umdec
            lda #$20            ; first copy target raster line in top field (then up, copying toward zero)
            sta PTopRastA
            lda #$38            ; raster offset for drawing new upper field line ($20 + $18)
            sta PTopRastD
            lda #$88            ; first copy target raster line of lower field (then up, copying toward zero)
            sta PBotRastA
            lda #$A0            ; raster offset for drawing new lower field line ($88 + $18)
            sta PBotRastD
            lda #$04            ; map offset back from HeroY for newly drawn line in top field.
            sta PTopMapOff
            lda #$01            ; remember that we are incrementing (will later added to PNudge for NudgePos)
            sta PInc
            bne :+
umdec:      lda #$38            ; first copy target raster line in lower field (then down, copying away from zero)
            sta PTopRastA
            lda #$20            ; raster offset for drawing new upper field line ($20 + 0)
            sta PTopRastD
            lda #$A0            ; first copy target raster line in lower field (then down, copying away from zero)
            sta PBotRastA
            lda #$88            ; raster offset for drawing new lower field line ($88 + 0)
            sta PBotRastD
            lda #$23            ; map offset back from HeroY for newly drawn line in top field.
            sta PTopMapOff
            lda #$00            ; remember that we are decrementing
            sta PInc
:           lda HeroY
            adc #$04
            and #$07            ; mod 8
            sta NudgePos        ; either what NudgePos will be (carry set) or what NudgePos was (carry clear)
            lda R_BANK          ; save bank
            sta BankSave        ; (but assume we are already in 1A00 ZP)
            lda #$00            ; go to bank 0, where (hires) graphics memory lives
            sta R_BANK
            sta TouchedVoid     ; reset "touched the void" flag            
            ; do the top field
            lda HeroY           ; find the new data line for the top field
            sec
            sbc PTopMapOff      ; counting back from HeroY to either top or bottom of top field
            bcs notvoid
            inc TouchedVoid     ; we have touched the void in the top field
notvoid:    sta MapOffset       ; store the map offset we will draw top field line from
            lda PTopRastA       ; first raster line processed in copy (inc=top, dec=bottom)
            clc
            adc NudgePos        ; newnudge/oldnudge
            ldy PInc            ; carry should still be clear
            bne :+              ; set carry if we we decrementing
            sec
:           jsr copylines       ; copy lines that can be copied
            lda PTopRastD       ; raster line that is target for new draw
            clc
            adc NudgePos        ; plus nudge
            tax                 ; move raster line to X for drawline
            ldy TouchedVoid     ; if we are in the void, draw the void
            beq :+              ; branch if we are not in the void
            jsr drawvoid
            jmp btmfield
:           lda MapOffset
            jsr drawline        ; draw line (map pointer is still in A, raster pointer is in X)
            ; do the bottom field
btmfield:   lda PBotRastA       ; first raster line processed in copy (inc=top, dec=bottom)
            clc
            adc NudgePos        ; newnudge/oldnudge
            ldy PInc            ; carry should still be clear
            bne :+              ; set carry if we are decrementing
            sec
:           jsr copylines       ; copy lines that can be copied
            lda PBotRastD       ; raster line that is target for new draw
            clc
            adc NudgePos        ; plus nudge
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
            lda PInc            ; advance PNudge to NudgePos (adds one if we were incrementing)
            beq updend
            inc NudgePos        ; will not bother making it mod 8 because setnudge will not care.
updend:     rts

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
            bcc lmprep          ; no chance carry got set
:           sbc #$08            ; no chance carry got cleared
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
; exits in HGR1 ZP since all callers want to go there immediately.
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
            pha                 ; re-stash so we can check for disk
            asl
            rol
            rol                 ; move color bits into lower two bits to serve as color index
            and #$03
            tax
            pla                 ; recall the map byte one last time
            and #$3F            ; filter out color bits
            cmp #C_DISK         ; if it is a disk, use the disk colors
            bne usemapcol
            lda DiskColors, x
            bne applycolor
usemapcol:  lda MapColors, x    ; load the indexed color
applycolor: and ZPxScratch      ; apply to the pixels
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

