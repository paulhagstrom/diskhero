; DISKHERO
; Apple III Hires region
; map display upper and lower block

; paint the whole map
; (updates afterwards are incremental, drawing single lines and using smooth scroll)
; This is designed to be able to paint under any circumstance, but it turns out that
; within the logic of the game it's only called once at the beginning.
; So all the logic about void regions will never get called upon unless it is decided
; later to randomize the hero's starting point.

initmap:    lda #$20            ; top field starts at absolute raster line $20
            sta CurScrLine      ; CurScrLine keeps track of the current absolute raster line
            lda HeroY           ; HeroY is the $23rd abstract line down
            sec                 ; so the map data pointer for the top raster line in the top field
            sbc #$23            ; is $23 above HeroY.
            bcs novoidup        ; we did not run off the edge, there is no upper void
            eor #$FF            ; invert the negative number to find the size of the upper void
            adc #$01
            tax                 ; put the vertical extent of the upper void in X
            lda #$00            ; start the map at zero when we get past the void
            beq :+
novoidup:   ldx #$00            ; no upper void, extent of the upper void is 0
:           stx VoidU           ; number of void lines above the map data
            jsr setmapptr       ; load mapptr for the first map data line we will be drawing
            cpx #$00            ; is there a void?
            beq hiresline       ; no, there is no void, just go start drawing
:           ldx CurScrLine      ; yes, there is a void, load the raster line into X
            jsr drawvoid        ; draw the void line
            inc CurScrLine      ; move down to the next raster line
            dec VoidU           ; if there are still void lines left, keep drawing them
            bpl :-
hiresline:  ldx CurScrLine      ; load the target raster line into X
            jsr drawlineb       ; we already set MapPtr earlier, use internal entry point
            inc CurScrLine      ; advance the graphics raster line
            lda MapPtrL         ; advance the map pointer to the next line
            clc
            adc #$40
            sta MapPtrL
            bcc novoiddown
            inc MapPtrH
            lda MapPtrH
            cmp #$60            ; did we just fall off the map into the bottom void?
            bne novoiddown      ; no, so continue on
:           ldx CurScrLine      ; note that if we are in the lower void, we must be in bottom field
            cpx #$A9            ; last line of bottom field complete? ($88-$A8)
            beq imdone
            jsr drawvoid        ; draw the void line
            inc CurScrLine      ; advance the graphics raster line
            jmp :-
novoiddown: ldx CurScrLine
            cpx #$A9            ; last line of bottom field complete? ($88-$A8)
            beq imdone          ; if so, we are finished drawing the hires map
            cpx #$40            ; last line of top field complete? ($20-$3F)
            bne hiresline       ; nope, keep going            
            lda #$88            ; we just finished the top field, so skip ahead to the lower field
            sta CurScrLine      ; set the raster line for the start of the lower field
            lda HeroY           ; set up map starting point for the top of the lower field
            clc                 ; which is 4 lines past HeroY.
            adc #$04
            bcs :+              ; are we already in a lower void? If so, the whole field is in the void.
            jsr setmapptr       ; not in the void, set up MapPtr for the data
            jmp hiresline       ; proceed to draw
:           ldx CurScrLine      ; there is nothing left but void, so just void the bottom field
            jsr drawvoid
            inc CurScrLine
            lda CurScrLine
            cmp #$A9            ; last line of bottom field complete? ($88-$A8)
            bne :-              ; nope, keep drawing void lines
imdone:     rts
            
; updatemap will effect a vertical movement of the map regions of the screen.
; if you call it with carry clear, it will move the map up (hero downward), increasing nudge
; if you call it with carry set, it will move the map down (hero upward), decreasing nudge
; it is assumed that the HeroY coordinate has just been changed, triggering this call.
; it will finish by setting NudgePos correctly so that the interrupt handler will display it right.

; in general, increasing nudge from oldnudge to oldnudge + 1
; copy graphics line 08 + oldnudge to 00 + oldnudge
; copy graphics line 10 + oldnudge to 08 + oldnudge
; copy graphics line 18 + oldnudge to 10 + oldnudge
; draw map line for graphics line 20 + oldnudge on graphics line 18 + oldnudge
; and then advance nudge to become oldnudge + 1
; to move back up (decreasing nudge from nudge + 1 to nudge)
; copy graphics line 10 + nudge to 18 + nudge
; copy graphics line 08 + nudge to 10 + nudge
; copy graphics line 00 + nudge to 08 + nudge
; draw map line for graphics line 00 + nudge on graphics line 00 + nudge
; top field starts at $20, draws $20 lines of map (offsets 0-1F),
; bottom field starts at $88, draws $20 lines of map (offsets 27-46).
; The zero point (no void, map offsets from 0-46 are rendered) has HeroY at 23.
; So, the top visible line is HeroY - 23,
; the last visible line in the upper field is HeroY - 4
; playfield goes from HeroY - 3 to HeroY + 3
; and bottom field goes from HeroY + 4 to HeroY + 23.
;
updatemap:  bcs umdec           ; if we decrementing nudge, skip past the incrementing parm block
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
            lda #$01            ; remember that we are incrementing (will later be added to PNudge for NudgePos)
            sta PInc
            bne umbegin         ; skip past the decrementing parm block
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
            lda #$00            ; remember that we are decrementing (will later be added to PNudge for NudgePos)
            sta PInc
umbegin:    lda HeroY
            adc #$04            ; intentionally not clearing carry before this, using the entry value of carry
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
            bcs umnotvoid
            inc TouchedVoid     ; we have touched the void in the top field
umnotvoid:  sta MapOffset       ; store the map offset we will draw top field line from
            lda PTopRastA       ; first raster line (inc=top, dec=bottom) in copy operation in top field
            clc
            adc NudgePos        ; newnudge/oldnudge
            ldy PInc            ; carry should still be clear
            bne :+              ; set carry if we we decrementing
            sec
:           jsr copylines       ; copy lines that can be copied
            lda PTopRastD       ; raster line that is target for new draw in top field
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
btmfield:   lda PBotRastA       ; first raster line (inc=top, dec=bottom) in copy operation in bottom field
            clc
            adc NudgePos        ; newnudge/oldnudge
            ldy PInc            ; carry should still be clear
            bne :+              ; set carry if we are decrementing
            sec
:           jsr copylines       ; copy lines that can be copied
            lda PBotRastD       ; raster line that is target for new draw in bottom field
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

; copylines uses self-modifying code and stack pushing to quickly copy the three graphics lines
; enter with graphics line (top line plus NudgePos), carry clear for increase nudge, set for decrease nudge
; exits with x still holding the line that would be the target of new draw (was last source)
; trying to squeeze as many cycles as possible out of this, so using the stack for copy target
; assumes we are already in bank 0 (video data), and sets ZP to $1A00 on exit 

LinesLeft:  .byte   0
SourceS:    .byte   0
TargS:      .byte   0
TargL:      .byte   0
TargHA:     .byte   0
TargHB:     .byte   0
CLStack:    .byte   0
CLEnv:      .byte   0

copylines:  tay                 ; move start line into Y
            tsx                 ; preserve stack state for recall when we are done
            stx CLStack
            lda R_ENVIRON
            sta CLEnv
            and #%11111011      ; set stack bit to zero to get alt stack
            sta R_ENVIRON
            lda #$02            ; move three lines, countdown in LinesLeft
            sta LinesLeft
            lda YHiresS, y
            sta TargS           ; target stack (end of line)
            lda YHiresHA, y
            sta TargHA          ; target A high
            lda YHiresHB, y
            sta TargHB          ; target B high
lmnext:     tya                 ; compute source line relative to target line
            bcs :+              ; if carry is set we are subtracting
            adc #$08            ; if carry is clear we are adding
            bcc lmprep          ; carry survives from entry (no chance carry got set)
:           sbc #$08            ; carry survives from entry (no chance carry got cleared)
lmprep:     tay
            lda YHiresL, y
            sta lmsrca + 1      ; modify code in upcoming loop
            sta lmsrcb + 1
            lda YHiresHA, y
            sta lmsrca + 2      ; source A high
            lda YHiresHB, y
            sta lmsrcb + 2      ; source B high
            lda YHiresS, y      ; stack pointer not used in source but passed on to be target after
            sta SourceS
            lda TargHA          ; point stack at target A page
            eor #$01            ; this is where ZP has to be, in order for stack to be where we want
            sta R_ZP            ; point ZP (thus stack)
            ldx TargS           ; start stack at the end of the target line
            txs
            ldx #$27
lmsrca:     lda $2000, x
            pha
            dex
            bpl lmsrca
            lda TargHB          ; point stack at target B page
            eor #$01            ; this is where ZP has to be, in order for stack to be where we want
            sta R_ZP            ; point ZP (thus stack)
            ldx TargS           ; start stack at the end of the target line
            txs
            ldx #$27
lmsrcb:     lda $4000, x
            pha
            dex
            bpl lmsrcb
            dec LinesLeft
            bmi lmdone          ; done moving lines
            lda lmsrca + 1      ; prior source becomes new target
            sta TargL
            lda SourceS
            sta TargS
            lda lmsrca + 2
            sta TargHA
            lda lmsrcb + 2
            sta TargHB
            jmp lmnext          ; carry should still be surviving from entry
lmdone:     lda CLEnv           ; restore environment, ZP, stack
            sta R_ENVIRON
            lda #$1A            ; restore ZP to $1A00
            sta R_ZP
            ldx CLStack
            txs
            rts

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

; table of map x-coordinates and the corresponding place to find them in a line, for use
; when we selectively update the screen display.  Indexed by x-coordinate in map row.

MapPix: .byte   $00, $00, $00, $00, $00, $00, $00
        .byte   $01, $01, $01, $01, $01, $01, $01
        .byte   $02, $02, $02, $02, $02, $02, $02
        .byte   $03, $03, $03, $03, $03, $03, $03
        .byte   $04, $04, $04, $04, $04, $04, $04
        .byte   $05, $05, $05, $05, $05, $05, $05
        .byte   $06, $06, $06, $06, $06, $06, $06
        .byte   $07, $07, $07, $07, $07, $07, $07
        .byte   $08, $08, $08, $08, $08, $08, $08
