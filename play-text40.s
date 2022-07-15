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
            
initplay:   ldy #$27            ; text lines 08-11: playfield (frame)
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
            rts
            
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
PlayLeft:   .byte   $00
PlayRight:  .byte   $00
PlStack:    .byte   $00     ; saved stack pointer
PlEnv:      .byte   $00     ; saved environment register
BorderR:    .byte   0
BorderV:    .byte   0
BorderRYet: .byte   0

drawplay:   
            ; the middle lores field starts at map $42 and draws to $46 (plus NudgePos)
            ; in order to keep hero in the middle, five columns are used by a frame
            ; based on hero position, 5 total, high nibble of HeroX of those are on the right
            ; (i.e. if HeroX is 32, there are 3 on the right, 2 on the left)
            lda R_ENVIRON       ; save evironment register
            sta PlEnv
            tsx
            stx PlStack
            and #%11111011      ; set stack bit to zero to get alt stack
            sta R_ENVIRON
            lda #$08            ; we are at the top of the playfield box (in the top border)
            sta CurScrLine
            lda HeroX           ; take high nibble of HeroX - that is BorderR
            lsr
            lsr
            lsr
            lsr
            sta BorderR         ; BorderR is how many columns (0-based) of border are on the right
            ; compute thumb boundaries
            lda HeroX           ; HeroX + $11 is the last drawn column inside the border
            clc
            adc #$11
            sta PlayRight       ; PlayRight is the map column of the right edge of visible playfield
            lda HeroX           ; HeroX - $11 is the first drawn column inside the border
            sec
            sbc #$11            ; could derive left void from this but might not be faster
            bpl :+              ; if that runs off the left edge of the map,
            lda #$00            ; record column as zero
:           sta PlayLeft        ; PlayLeft is the map column of the left edge of visible playfield (or 0)
            ; compute the voids (areas in the display but off the edges of the map)
            eor #$FF
            clc
            adc #$01            ; invert the left map column (that is, compute $11 - HeroX)
            bpl :+              ; if it is positive, it is the left void
            lda #$00            ; otherwise there is no left void
:           sta VoidL
            lda HeroX           ; if HeroX - $23 fails to go off the map, it's close enough to the
            sec                 ; right for there to be a right void
            sbc #$2E
            bpl :+              ; there is a right void
            lda #$00
:           sta VoidR
            lda HeroY           ; check for top void (only a chance if HeroY is in the top half of map)
            bmi novoidu         ; branch away (no upper void) if HeroY is in the bottom half of the map
            lda #$03            ; if 3 - HeroY is positive, there is a top void
            sec
            sbc HeroY
            bpl :+              ; there is a top void
novoidu:    lda #$00
:           sta VoidU
dppostvoid: ; start drawing with top and bottom borders (for thumb). Just colors, chars will already be there
            ldy CurScrLine
borderh:    lda YLoresHB, y     ; $800 base (color space)
            eor #$01            ; where the ZP needs to be for the stack to be where we want it
            sta R_ZP            ; point stack at color page
            lda YLoresS, y      ; low byte of the address of the end of this line
            tax
            txs                 ; point stack pointer at end of the line
            lda PlayRight       ; move thumb parms into screen holes to speed up access inside loop
            sta ZThumbTogg
            lda PlayLeft
            sta ZThumbNext
            ldy #$27            ; paint 28 characters
            lda #$57            ; with grey1 background
borderhb:   pha                 ; push into the color space
            dey
            bmi borderhz        ; branch away if drawing is done
            tax                 ; stash color in x
            lda ThumbXlate, y   ; what map column are we entering?
            bmi borderfix       ; this column has a fixed color
            cmp ZThumbTogg      ; did we just pass the toggle?
            bcc borderhtog      ; yes, go change the color
            txa                 ; retrieve color from x
            jmp borderhb        ; and add it to the line
borderfix:  lda #$57            ; switch color to brighter gray
            jmp borderhb        ; and add it to the line
borderhtog: lda ZThumbNext      ; arm new left side toggle
            sta ZThumbTogg
            lda #$00            ; disable left side toggle
            sta ZThumbNext
            txa                 ; retrieve color from x
            eor #$A7            ; and swap color
            jmp borderhb        ; then add it to the line
borderhz:   lda CurScrLine
            cmp #$09            ; if we have done both top and bottom
            beq innerplay       ; move on to the middle
            lda #$50            ; reset the color
            inc CurScrLine      ; set exit condition for next time (borderh does not use the value)
            ldy #$10            ; do the bottom line (Y holds the current screen line for borderh)
            jmp borderh
innerplay:  lda #$1A            ; return ZP and stack to 1A/true
            sta R_ZP
            lda PlEnv
            sta R_ENVIRON
            ldx PlStack
            txs
            lda HeroY           ; find map pointer for top non-void line
            clc
            adc VoidU           ; factor out upper void
            sec
            sbc #$03
            jsr setmapptr
burnvoidu:  dec VoidU           ; burn through upper void lines first if there are any
            bmi pfline          ; branch away if done with upper void
            jsr playvoid        ; draw the void at CurScrLine
            inc CurScrLine
            jmp burnvoidu
pfline:     jsr loresline       ; draw the current MapPtr map line at CurScrLine
            lda MapPtrL         ; advance map pointer
            clc
            adc #$40
            sta MapPtrL
            bcc :+
            inc MapPtrH
            lda MapPtrH
            cmp #$60
            beq burnvoidd       ; just ticked into a lower void (MapPtr went off the map)
            ; advance screen line
:           inc CurScrLine
            lda CurScrLine
            cmp #$10
            bne pfline          ; more lines to draw, go draw them
pfdone:     rts
burnvoidd:  inc CurScrLine
            lda CurScrLine
            cmp #$10
            beq pfdone          ; branch away if we have drawn all lines above lower border
            jsr playvoid        ; draw the void at CurScrLine
            jmp burnvoidd

; draw a void line in the playfield
; TODO - maybe can refactor so I do not have border logic in both loresline and playvoid
playvoid:   lda #$04            ; draw five border columns total
            sta BorderV
            lda BorderR         ; save a local copy of this that we can decrement
            sta BorderRYet
            ldy CurScrLine
            lda YLoresS, y      ; address of the right edge of the line
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
loreschar:  cpy #$3F            ; check to see if we're off the right edge of the map
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
useplaycol: lda MapColors, x    ; load the indexed color
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
