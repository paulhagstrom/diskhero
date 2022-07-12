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

domove:     lda #$82            ; we will use ZPtrA, ZPtrB, ZPtrC
            sta ZPtrA + XByte   ; so set up the XByte for all three up here
            sta ZPtrB + XByte   ; so we do not wind up setting them repeatedly
            sta ZPtrC + XByte   ; inside a loop
            lda HeroX           ; start with the hero
            sta OldX
            lda HeroY
            sta OldY
            lda VelocityX
            sta VelX
            lda VelocityY
            sta VelY
            jsr trymove
            bcs herodone        ; the move failed, do not need to do a map update
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
            lda (ZHoardTick), y ; ready to move yet?
            lsr
            beq ticksdone
            sta (ZHoardTick), y
            jmp nexthoard
ticksdone:  lda (ZHoardSp), y   ; reset ticks for next move
            sta (ZHoardTick), y
            lda (ZHoardXX), y   ; stash second segment X-coordinate
            sta ZPtrC           ; in ZPtrC for easy retrieval later
            lda (ZHoardXV), y
            sta VelX
            lda (ZHoardYV), y
            sta VelY
            bne hmoving
            lda VelX            ; Y velocity was 0, is X velocity 0 also?
            bne hmoving
            ldx Seed            ; if hoarder was not moving
            lda Random, x       ; send it in a random direction
            bpl sendhoriz       ; send it horizontally
            inx
            lda Random, x
            stx Seed
            sta VelY
            lda #$00
            sta VelX
            beq hmoving
sendhoriz:  inx
            lda Random, x       ; send it horizontally
            stx Seed
            sta VelX
            lda #$00
            sta VelY
hmoving:    lda (ZHoardY), y
            sta OldY
            lda (ZHoardX), y
            sta OldX
            jsr trymove
            bcc hmoved          ; the move succeded
            ldy CurrHoard       ; the move failed, zero out the velocity
            lda #$00            ; (will be sent in another direction next cycle)
            sta (ZHoardXV), y
            sta (ZHoardYV), y
            bne nexthoard
hmoved:     ldy CurrHoard       ; remove old hoarder from the map
            lda (ZHoardYY), y   ; replace second segment (head) with zero
            jsr setmapptr       ; locate original y-coordinate on map
            ldy ZPtrC           ; we stashed second segment X coordinate in here earlier
            lda MapPtrL
            sta ZPtrC
            lda MapPtrH
            sta ZPtrC + 1       ; X-Byte was set up top
            lda #$00            ; remove second segment from the map
            sta (ZPtrC), y
            lda #C_HHEADA       ; put head in old first segment (hands) position
            ldy OldX
            sta (ZPtrB), y
            ldy CurrHoard
            lda NewX
            sta (ZHoardX), y
            lda NewY
            sta (ZHoardY), y
            lda VelX
            sta (ZHoardXV), y
            lda VelY
            sta (ZHoardYV), y
            lda OldX
            sta (ZHoardXX), y
            lda OldY
            sta (ZHoardYY), y
            cmp NewY
            bcc handsdown       ; OldY (head) is less than NewY (hands), so point down
            lda OldX
            cmp NewX
            bcc handsright      ; OldX (head) is less than NewX (hands), so point right
            beq handsup         ; OldX and NewX are the same, and OldY is not less than NewY.
            lda #C_HHANDLA      ; so point up
            bne handoff
handsup:    lda #C_HHANDUA
            bne handoff
handsright: lda #C_HHANDRA
            bne handoff
handsdown:  lda #C_HHANDDA
handoff:    ldy NewX
            sta (ZPtrA), y      ; update the map
nexthoard:  dec CurrHoard
            bmi donehoard
            jmp movehoard
donehoard:
            jsr drawplay        ; redraw middle playfield
            ; scroll if we need to
            ; YOU ARE HERE - something is messed up with this.  If I hit something it gets messy. OR something.
            ; Something no longer works with updatemap.
            ; Also: the hoarders seem to detect collisions one step too late.  They'll turn AFTER clobbering something.
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
            sta ZPtrB + 1       ; X-Byte was set up top
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
            sta ZPtrA + 1       ; X-Byte was set up top
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
