; DISKHERO
; Apple III 40-column text region
; middle primary play field display
; occupies scan lines 40-87, text lines 08-10.
; display map lines 20-26 (where the top of the upper playfield is 0)
;           (or, in other words, up 3 and down 3 from the center)

BorderChar  = $00       ; C_SPACE
BorderColA  = $AF       ; grey2 background
BorderColB  = $5F       ; grey1 background

FrameText:  .byte C_WALL_RUD, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H
            .byte C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H
            .byte C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H
            .byte C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H
            .byte C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H
            .byte C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H
            .byte C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H
            .byte C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_H, C_WALL_LUD
            
; the only static stuff needing initialization are top/bottom border characters
; use ZP to stuff the frame characters in.

IpZPSave:   .byte   0

initplay:   lda R_ZP
            sta IpZPSave
            ldy #$08
            sec
iploop:     lda YLoresHA, y
            sta R_ZP
            ldx YLoresS, y
            ldy #$27
:           lda FrameText, y
            sta Zero, x
            dex
            dey
            bpl :-
            bcc ipdone
            ldy #$10
            clc
            jmp iploop
ipdone:     lda IpZPSave
            sta R_ZP
            rts
            
; translation table between text column and hires map column, when drawing passes from right
; scrollbar thumb toggles state.  $80 indicates a fixed (frame) color.
ThumbXlate: .byte   $80, $80
            .byte   $01, $03, $05, $07, $08, $0A, $0C, $0E
            .byte   $0F, $11, $13, $15, $16, $18, $1A, $1C
            .byte   $1D, $1E, $20, $21, $22, $24, $26, $28
            .byte   $29, $2B, $2D, $2F, $30, $32, $34, $36
            .byte   $37, $39, $3B, $3D
            .byte   $80, $80
PlayLeft:   .byte   0
PlayRight:  .byte   0
PlayTop:    .byte   0
PlStack:    .byte   0       ; saved stack pointer
PlEnv:      .byte   0       ; saved environment register
BorderV:    .byte   0       ; playfield border counter (total vertical border columns)
BorderR:    .byte   0       ; playfield border counter (total border columns on the right)
BorderRYet: .byte   0       ; playfield border counter (right border columns yet to draw)
BorderL:    .byte   0       ; memory index of rightmost playfield column offset from YLoresL after border done
BorderS:    .byte   0       ; memory index of leftmost playfield column from YLoresS after border done
BorDataA:   .byte   0       ; character or color inside border on left and right
BorDataB:   .byte   0       ; character or color on border on left and right

; the middle lores field starts at map $42 and draws to $46 (plus NudgePos)
; in order to keep hero in the middle, five columns are used by a frame
; based on hero position, 5 total, high nibble of HeroX of those are on the right
; (i.e. if HeroX is 32, there are 3 on the right, 2 on the left)

drawplay:   lda #$08            ; we are at the top of the playfield box (in the top border)
            sta CurScrLine
            lda HeroX           ; take high nibble of HeroX - that is BorderR
            lsr
            lsr
            lsr
            lsr
            sta BorderR         ; BorderR is how many columns (0-based) of border are on the right
            lda HeroX           ; HeroX + $11 is the last drawn column inside the border
            clc
            adc #$11
            sta PlayRight       ; map column of the right edge of visible playfield (could be off the map)
            sec
            sbc #$23            ; back 23 from the last drawn column is the first drawn column
            bcs pmnovoidl       ; if it is still on the map, there is no left void, skip ahead
            eor #$FF            ; inverse of the number is the left void
            adc #$01
            sta VoidL
            lda #$00
            sta PlayLeft
            beq :+
pmnovoidl:  sta PlayLeft
            lda #$00
            sta VoidL
:           lda PlayRight       ; if PlayRight - #$3E (last map column) is positive, that is VoidR
            sec
            sbc #$3E
            bmi pmnovoidr       ; branch away if there is no VoidR
            sta VoidR
            lda #$3E
            sta PlayRight
            bne :+
pmnovoidr:  lda #$00
            sta VoidR
:           lda HeroY           ; check for top void
            sec
            sbc #$03
            bcs pmnovoidu       ; branch away if there is no top void
            eor #$ff
            adc #$01
            sta VoidU
            lda #$00
            sta PlayTop
            jmp dppostvoid
pmnovoidu:  sta PlayTop
            lda #$00
            sta VoidU
dppostvoid: ; start drawing with top and bottom borders (for thumb). Just colors, chars will already be there
            lda R_ENVIRON       ; save evironment register
            sta PlEnv
            tsx
            stx PlStack
            and #%11111011      ; set stack bit to zero to get alt stack
            sta R_ENVIRON
            ldy CurScrLine
borderh:    lda YLoresHB, y     ; $800 base (color space)
            eor #$01            ; where the ZP needs to be for the stack to be where we want it
            sta R_ZP            ; point stack at color page
            ldx YLoresS, y      ; low byte of the address of the end of this line
            txs                 ; point stack pointer at end of the line
            lda PlayRight
            sta ZThumbTogg
            lda PlayLeft
            sta ZThumbNext
            ldy #$27            ; paint 28 characters
borderhb:   ldx ThumbXlate, y
            bmi bordfixed
            cpx ZThumbTogg      ; did we just pass the toggle value?
            bcc bordtogg        ; yes, go change the color
bordpush:   pha
            dey
            bpl borderhb
            ldx CurScrLine
            cpx #$09            ; if we have done both top and bottom
            beq innerplay
            inc CurScrLine      ; set exit condition for next time (borderh does not use the value)
            ldy #$10            ; do the bottom line (Y holds the current screen line for borderh)
            jmp borderh
bordfixed:  lda #$57            ; fixed color is brighter gray
            jmp bordpush
bordtogg:   ldx ZThumbNext      ; arm new left side toggle
            stx ZThumbTogg
            ldx #$00
            stx ZThumbNext
            eor #$A7            ; swap color
            jmp bordpush
innerplay:  lda #$1A            ; return ZP and stack to 1A/true
            sta R_ZP
            lda PlEnv
            sta R_ENVIRON
            ldx PlStack
            txs
burnvoidu:  dec VoidU           ; burn through upper void lines first if there are any
            bmi pfstart         ; branch away if done with upper void
            jsr playvoid        ; draw the void at CurScrLine
            inc CurScrLine
            jmp burnvoidu
pfstart:    lda PlayTop         ; get address of first drawn line
            jsr setmapptr
pfline:     lda MapPtrH         ; see if we are in the four lines below the map
            cmp #$60
            beq :+              ; if so, branch away, else:
            jsr loresline       ; draw the current MapPtr map line at CurScrLine
            jmp pfnext          ; and advance
:           jsr playvoid        ; draw a void line if we're in the void
pfnext:     lda MapPtrL         ; advance map pointer
            clc
            adc #$40
            sta MapPtrL
            bcc :+
            inc MapPtrH
:           inc CurScrLine      ; move to the next screen line
            lda CurScrLine
            cmp #$10            ; have we already done the last?
            bne pfline          ; if not (more to draw), go up and do them
            rts

; draw the playfield border and compute edges of line/void to draw
; after: BorderL, BorderS hold the low bytes of the screen memory addresses for line (left, right)
playbord:   lda #BorderChar
            sta BorDataA
            sta BorDataB
            ldy CurScrLine
            lda YLoresHA, y     ; $400 base (char space)
pbdraw:     sta R_ZP            ; point ZP at appropriate space
            lda #$04            ; draw five border columns total
            sta BorderV
            lda BorderR         ; save a local copy of this that we can decrement
            sta BorderRYet
            ldx YLoresS, y      ; address of the right edge of the line
            lda BorDataB        ; draw right border
:           sta Zero, x
            lda BorDataA        ; switch to inside value
            dex
            dec BorderV
            dec BorderRYet
            bpl :-
            stx BorderS         ; this is where line or void will start (right edge)
            ldx YLoresL, y      ; address of the left edge of the line
            lda BorDataB        ; draw left border
:           sta Zero, x
            lda BorDataA        ; switch to inside value
            inx
            dec BorderV
            bpl :-
            stx BorderL         ; this is where line of void will start (left edge)
            ; if we didn't just do border colors, do border colors
            lda BorDataA
            cmp #BorderColA
            beq :+
            lda #BorderColB
            sta BorDataB
            lda #BorderColA
            sta BorDataA
            ldy CurScrLine
            lda YLoresHB, y     ; $800 base (color space)
            jmp pbdraw
:           lda #$1A            ; put ZP back
            sta R_ZP
            rts
            
; draw a void line in the playfield
playvoid:   jsr playbord        ; draw the border
            ldy CurScrLine
            lda YLoresHB, y     ; $800 base (color space)
            sta R_ZP            ; go to color space
            lda BorderL
            sta pvcol + 1
            lda #$10            ; magenta background, black foreground
            ldx #$22            ; drawing 34 columns between borders
pvcol:      sta Zero, x
            dex
            bpl pvcol
            lda YLoresHA, y     ; $800 base (char space)
            sta R_ZP            ; go to char space
            lda BorderL
            sta pvchar + 1
            lda #C_SPACE        ; void contains C_SPACE
            ldx #$22            ; drawing 34 columns between borders
pvchar:     sta Zero, x
            dex
            bpl pvchar
            lda #$1A            ; put ZP back
            sta R_ZP
            rts

; draw a lores line in the playfield, assumes ZP is 1A00 and MapPtr is set.
loresline:  jsr playbord        ; draw the border
            lda MapPtrL
            sta ZMapPtr
            lda MapPtrH
            sta ZMapPtr + 1
            lda #$82
            sta ZMapPtr + XByte
            ; store the text data in $1A00 ZP (color) and $1B00 stack (char) as we build it
            ldx #$22            ; we are drawing 34 characters across
            ldy VoidR           ; if there is void to the right, draw it
            beq pfpostvoid
:           lda #$10            ; magenta background, black foreground
            sta Zero, x
            lda #C_SPACE
            pha
            dex
            dey
            bpl :-
pfpostvoid: stx ZPxScratch
            ldy PlayRight
pfmapdraw:  lda (ZMapPtr), y
            sta ZCharTemp
            and #%00110000      ; test to see if this is 0-F (separate color info)
            beq :+              ; branch if this is an element with an indexed color
            ldx ZCharTemp       ; this is an element with an intrinsic color (in ZFontCol)
            lda ZFontCol, x     ; so look it up
            jmp gotcolor
:           lda ZCharTemp
            asl
            rol
            rol                 ; move color bits into lower two bits to serve as color index
            and #$03
            tax
            lda ZCharTemp
            and #$3F            ; strip the color bits
            sta ZCharTemp
            cmp #C_DISK         ; if it is a disk, use the disk colors
            bne useplaycol
            lda DiskColors, x
            jmp gotcolor
useplaycol: lda MapColors, x    ; load the indexed color
gotcolor:   and #$0F            ; keep only the foreground color (background = black/0)
gotcolorb:  ldx ZPxScratch
            sta Zero, x         ; store color in ZP (1A00)
            lda ZCharTemp
            pha                 ; push character to stack
            dey
            bmi leftvoid        ; oops, we have reached the left void
            dec ZPxScratch
            dex
            bpl pfmapdraw
            bmi pfdone
leftvoid:   dex                 ; we touched the void, any left to draw?
            bmi pfdone          ; nope, all finished
            lda #C_SPACE        ; left edge void, push a magenta blank
            pha
            lda #$10            ; magenta
            sta Zero, x
            jmp leftvoid
pfdone:     ldy CurScrLine      ; now, send it to the screen
            lda YLoresHA, y     ; $400 base (character space)
            sta R_ZP            ; go to character memory
            lda BorderL
            sta pfwchar + 1
            ldx #$00
:           pla
pfwchar:    sta Zero, x
            inx
            cpx #$23
            bne :-
            lda YLoresHB, y     ; $800 base (color space)
            sta R_ZP
            lda BorderL
            sta pfwcol + 1
            ldx #$22
:           lda Zero1A, x
pfwcol:     sta Zero, x
            dex
            bpl :-
            lda #$1A
            sta R_ZP            ; go back to 1A00 ZP
            rts
