; DISKHERO
; movement processing
;
; includes antagonist movement


VelX:       .byte   0
VelY:       .byte   0
OldX:       .byte   0
OldY:       .byte   0
NewX:       .byte   0
NewY:       .byte   0
CurrHoard:  .byte   0
ScrollUp:   .byte   0
ScrollDown: .byte   0

domove:     lda HeroX       ; start with the hero
            sta OldX
            lda HeroY
            sta OldY
            lda VelocityX
            sta VelX
            lda VelocityY
            sta VelY
            jsr trymove
            bcs herodone        ; the move failed
            lda #$00            ; remove old hero from map
            sta ScrollUp        ; reset scroll up/down triggers
            sta ScrollDown
            ldy OldX
            sta (ZPtrB), y
            lda #C_HERO         ; put new hero on map
            ldy NewX
            sty HeroX           ; record new X location
            sta (ZPtrA), y      ; update the map
            ldy NewY
            sty HeroY           ; record new Y location
            lda VelX
            sta VelocityX       ; record new X velocity
            lda VelY
            sta VelocityY       ; record new Y velocity
            beq herodone        ; scroll the map if we need to
            bmi scrolldown
            inc ScrollUp
            bne herodone
scrolldown: inc ScrollDown
herodone:            
            ; move hoarders
            ldy NumHoards
            sty CurrHoard
movehoard:  ldy CurrHoard
            lda (ZHoardY), y
            sta OldY
            lda (ZHoardX), y
            sta OldX
            lda (ZHoardXV), y
            sta VelX
            lda (ZHoardYV), y
            sta VelY
            jsr trymove
            bcs nomove          ; the move failed
            lda #$00            ; remove old hoarder from the map
            ldy OldX
            sta (ZPtrB), y      ; just removing the head?
            ldy CurrHoard
            lda NewX
            sta (ZHoardX), y
            lda NewY
            sta (ZHoardY), y
            lda VelX
            sta (ZHoardXV), y
            lda VelY
            sta (ZHoardYV), y
            lda #C_HHEADA       ; put new head on map
            ldy NewX
            sta (ZPtrA), y      ; update the map
nomove:     dec CurrHoard
            bpl movehoard
            
            jsr drawplay        ; redraw middle playfield
            ; scroll if we need to
            lda ScrollUp
            beq :+
            clc
            jmp updatemap
:           lda ScrollDown
            beq :+
            sec
            jmp updatemap
:           rts
            
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

trymove:    ldx OldX
            lda VelX
            beq xchecked
            bmi xleft
            inx
            cpx #$3F
            bne xchecked
            dex                 ; ran off the right edge, stop horizontal motion
            lda #$00
            sta VelX
            beq xchecked
xleft:      dex
            bpl xchecked
            inx                 ; ran off the left edge, stop horizontal motion
            stx VelX
xchecked:   stx NewX
            lda OldY
            tax
            jsr setmapptr       ; locate original y-coordinate on map
            lda MapPtrL         ; and store in ZPtrB
            sta ZPtrB
            lda MapPtrH
            sta ZPtrB + 1
            lda #$82
            sta ZPtrB + XByte
            lda VelY
            beq ychecked
            bmi yup
            inx
            bne ychecked
            stx VelY            ; ran off the bottom, stop vertical motion
            dex
            bne ychecked
yup:        dex
            cpx #$FF
            bne ychecked
            inx                 ; ran off the top, stop vertical motion
            stx VelY
ychecked:   stx NewY
            txa
            jsr setmapptr       ; locate new y-coordinate on map
            lda MapPtrL         ; and store in ZPtrA
            sta ZPtrA
            lda MapPtrH
            sta ZPtrA + 1
            lda #$82
            sta ZPtrA + XByte
            ldy NewX
            lda (ZPtrA), y      ; look at NewX, NewY (where we are trying to move)
            and #$3F            ; color bits don't block movement
            beq moveclear       ; clear to move, nothing in the way
            cmp #C_DISK         ; disk is the only non-obstacle
            beq diagdisk        ; moved atop a disk
            ; we have hit something trying to move in the intended direction
            lda VelX            ; see if we were attempting to move horizontally
            beq :+              ; if not, skip horizontal check
            lda (ZPtrB), y      ; check NewX, OldY
            and #$3F            ; color bits don't block movement
            beq horizok         ; block was only vertical, we can move in the horiz
            cmp #C_DISK         ; disk is the only non-obstacle
            beq horizdisk       ; horizontal move lands on a disk
:           lda VelY            ; see if we were attempting to move vertically
            beq :+              ; if not, skip vertical check
            ldy OldX            ; check OldX, NewY
            lda (ZPtrA), y
            and #$3F            ; color bits don't block movement
            beq vertok          ; block was only horizontal, we can move in the vert
            cmp #C_DISK         ; disk is the only non-obstacle
            beq vertdisk        ; vertical move lands on a disk
            ; we have been stopped, move cannot be accomplished
:           lda #$00
            sta VelX
            sta VelY
            jmp movenope
horizdisk:  lda (ZPtrB), y      ; reload disk because we tossed the type/color bits
            jsr gotdisk
horizok:    lda #$00            ; stop vertical movement
            sta VelY
            lda OldY            ; new hero Y is unchanged
            sta NewY
            lda ZPtrB           ; map line pointer for new Y is same as old Y
            sta ZPtrA
            lda ZPtrB + 1
            sta ZPtrA + 1
            jmp moveclear
vertdisk:   lda (ZPtrA), y      ; reload disk because we tossed the type/color bits
            jsr gotdisk
vertok:     lda #$00            ; stop horizontal movement
            sta VelX
            lda OldX            ; new hero X is unchanged
            sta NewX
            jmp moveclear
diagdisk:   lda (ZPtrA), y      ; reload disk because we tossed the type/color bits
            jsr gotdisk
moveclear:  clc                 ; return with clear carry if move succeeds
            rts
movenope:   sec                 ; return with set carry if move fails
            rts

; called if moving player/antagonist lands on a disk
; assumes that the map byte is in A on entry.
gotdisk:    and #$C0            ; disk type
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
            ; removing the disk is unnecessary because the hero/antagonist will replace it
            rts
