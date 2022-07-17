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
OldOldX:    .byte   0
OldOldY:    .byte   0
IsHero:     .byte   0
CurrHoard:  .byte   0
CurrDisk:   .byte   0
ScrollUp:   .byte   0
ScrollDown: .byte   0
TargX:      .byte   0
TargY:      .byte   0
TargV:      .byte   0
TargD:      .byte   0
TargDX:     .byte   0
TargDY:     .byte   0
TargDTemp:  .byte   0

domove:     lda #$82            ; we will use ZPtrA, ZPtrB, ZPtrC
            sta ZPtrA + XByte   ; so set up the XByte for all three up here
            sta ZPtrB + XByte   ; so we do not wind up setting them repeatedly
            sta ZPtrC + XByte   ; inside a loop
            ; move hero
            sta IsHero          ; determines sound and score effects of hitting a disk
            lda #$00            ; reset scroll up/down triggers
            sta ScrollUp
            sta ScrollDown
            lda HeroX
            sta OldX
            lda HeroY
            sta OldY
            lda VelocityX
            sta VelX
            lda VelocityY
            sta VelY
            jsr trymove
            bcs herostop        ; the move failed, do not need to do a map update
            lda #$00            ; remove old hero from map
            ldy OldX
            sta (ZPtrB), y
            lda #C_HERO         ; put new hero on map
            ldy NewX
            sty HeroX           ; record new X location
            sta (ZPtrA), y      ; update the map
            ldy NewY
            sty HeroY           ; record new Y location
            ; would update on screen here except hero is never on the hires screen
herostop:   lda VelX
            sta VelocityX       ; record new X velocity
            lda VelY
            sta VelocityY       ; record new Y velocity
            beq herodone        ; skip scrolling the map if there was no Y movement
            bmi scrolldown      ; if Y velocity is negative (hero moves up), map scrolls down
            inc ScrollUp        ; if Y velocity is positive (hero moves down), map scrolls up
            jmp herodone
scrolldown: inc ScrollDown
herodone:   lda #$00            ; move hoarders
            sta IsHero          ; determines sound and score effects of hitting a disk
            ldy NumHoards
            sty CurrHoard
movehoard:  ldy CurrHoard
            lda (ZHoardTick), y ; ready to move yet? (up to 8 tick delay for each, governing speed)
            lsr
            beq ticksdone
            sta (ZHoardTick), y ; not ready to move, decrease ticks and move to check next hoarder
            jmp nexthoard
ticksdone:  lda (ZHoardSp), y   ; this one can move now, reset ticks for next move after this one
            sta (ZHoardTick), y
            lda (ZHoardXX), y   ; stash second segment (head) X-coordinate
            sta ZXXTemp         ; in ZXXTemp for easy retrieval later
            ldx Seed            ; random chance that a hoarder will stop spontaneously
            lda Random, x
            inx
            stx Seed
            and #$07            ; one in 8 chance hoarder stops (maybe changes directions)
            bne hoardcont
            lda #$00            ; horder stopped and may change direction.
            sta VelY
            sta VelX
            beq hoardredir
hoardcont:  lda (ZHoardXV), y
            sta VelX
            lda (ZHoardYV), y
            sta VelY
            beq hnovely         ; if there was Y velocity, hoarder is moving
            jmp hmoving
hnovely:    lda VelX            ; Y velocity was 0, is X velocity 0 also?
            beq hnotmoving  ; if there is X velocity, hoarder is moving
            jmp hmoving
            ; swap head and hands if hoarder was stopped, so it does not corner itself
hnotmoving: lda ZXXTemp         ; old head X-coordinate
            pha                 ; stash it as we sawap
            lda (ZHoardX), y    ; get the old hands x-coordinate
            sta (ZHoardXX), y   ; and put it in old head x-coordinate
            sta ZXXTemp         ; which we also are continuing to save for later use
            pla                 ; recall old head X-coordinate
            sta (ZHoardX), y    ; and put it in old hands x-coordinate
            lda (ZHoardY), y    ; then do the same for the Y coordinate
            pha
            lda (ZHoardYY), y
            sta (ZHoardY), y
            pla
            sta (ZHoardYY), y
hoardredir: ldx Seed            ; if hoarder was not moving
            lda Random, x       ; send it in a new direction
            inx
            stx Seed
            and #$03            ; one out of 4 chance it goes in a random direction
            bne hoardseek       ; otherwise seeks high value disk
            jmp hranddir        ; go forth randomly
hoardseek:  lda #$FF            ; scan disks to find closest highest value one
            sta TargD
            lda #$00
            sta TargX
            sta TargY
            sta TargV
            ldy NumDisks
            sty CurrDisk
dcheckdist: ldy CurrDisk
            lda (ZDiskType), y
            cmp TargV
            bcc dnotmore
            lda (ZDiskX), y     ; higher value than ones previously seen
            sta TargX           ; store this as the new target
            sta TargY
            jmp dnotbetter
dnotmore:   ldy CurrDisk
            lda (ZDiskX), y
            ldy CurrHoard
            sec
            sbc (ZHoardX), y
            sta TargDX          ; will be negative if disk is to the left
            bcs dxdpos
            eor #$FF
            adc #$01
dxdpos:     sta TargDTemp
            ldy CurrDisk
            lda (ZDiskY), y
            ldy CurrHoard
            sec
            sbc (ZHoardY), y
            sta TargDY          ; will be negative if disk is above
            bcs dydpos
            eor #$FF
            adc #$01
dydpos:     clc
            adc TargDTemp       ; very distant could seem close. Shrug.
            cmp TargD
            bcc dnotbetter
            sta TargD           ; this disk is closer than other same value
            ldy CurrDisk
            lda (ZDiskX), y     ; so make this the new target
            sta TargX
            lda (ZDiskY), y
            sta TargY
dnotbetter: dec CurrDisk
            bpl dcheckdist
            lda TargDX          ; velocity is just pos/neg/zero anyway, so store
            sta VelX
            lda TargDY
            sta VelY
            jmp hmoving         ; and go
hranddir:   lda Random, x       ; send it in a random direction
            bpl sendhoriz       ; horizontal/vertical choice yields: horizontal
            inx
            lda Random, x       ; send it vertically in a random direction
            stx Seed
            sta VelY
            lda #$00
            sta VelX
            beq hmoving
sendhoriz:  inx
            lda Random, x       ; send it horizontally in a random direction
            stx Seed
            sta VelX
            lda #$00
            sta VelY
hmoving:    lda CurrHoard
            lda (ZHoardY), y
            sta OldY
            lda (ZHoardX), y
            sta OldX
            jsr trymove
            bcc hmoved          ; the move succeded
            ldy CurrHoard       ; the move failed, zero out the velocity
            lda #$00            ; (will be sent in another direction next cycle)
            sta (ZHoardXV), y
            sta (ZHoardYV), y
            beq nexthoard
hmoved:     ldy CurrHoard
            lda (ZHoardAnim), y ; toggle animation frames
            eor #$01
            sta (ZHoardAnim), y
            sta ZFrame          ; stash in ZFrame for use in adding in later
            lda (ZHoardYY), y   ; replace second segment (head) with zero
            sta OldOldY
            jsr setmapptr       ; locate original second segment y-coordinate on map
            ldy ZXXTemp         ; we stashed second segment X coordinate in here earlier
            sty OldOldX
            lda MapPtrL
            sta ZPtrC
            lda MapPtrH
            sta ZPtrC + 1       ; X-Byte was set up top
            lda #$00            ; remove second segment from the map
            sta (ZPtrC), y            
            lda #C_HHEADA       ; put head in old first segment (hands) position
            clc
            adc ZFrame          ; select animation frame
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
            clc
            adc ZFrame          ; select animation frame
            sta (ZPtrA), y      ; update the map
            jsr gmupdscr        ; update the screen
nexthoard:  dec CurrHoard
            bmi donehoard
            jmp movehoard
donehoard:
            jsr drawplay        ; redraw middle playfield
            ; scroll if we need to
            ; YOU ARE HERE - something is messed up with this.  If I hit something it gets messy. OR something.
            ; Something no longer works with updatemap.
            ; Also: Hoarder collisions with Hero don't work quite right.  Sometimes pass through, sometimes stomp.
            ; Collisions will wall also seem to wind up removing the head until can be redirected.
            ; Somehow need to hold off on removing the second segment until a move succeeds.
            ; Hoarders' horizontal collisions with walls are currently eating up the walls again, too.
            lda ScrollUp
            beq :+
            clc
            jmp updatemap
:           lda ScrollDown
            beq :+
            sec
            jmp updatemap
:           rts

; update the screen where things moved
; later make this just collect things that will all be updated en masse at the end
; maybe consolating complications and avoiding redundancy
; not even sure that this is right (oldx, oldy), might this mess up on some hoarder moves
; since they are two segments long?
gmupdscr:   lda OldOldY     ; where the head was (segment 2)
            ldy OldOldX
            jsr updsingle
            lda OldY        ; where the hands were
            ldy OldX
            jsr updsingle
            lda NewY        ; where the hands are
            ldy NewX
            jsr updsingle
            rts

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
            beq xchecked        ; if not moving horizontally, need not check
            bmi xleft           ; if moving left, jump to leftward check
            inx
            cpx #$3F            ; check to see if moving right goes off the map
            bne xchecked
            dex                 ; ran off the right edge, stop horizontal motion
            lda #$00
            sta VelX
            jmp xchecked
xleft:      dex                 ; check to see if moving left goes off the map
            bpl xchecked
            inx                 ; ran off the left edge, stop horizontal motion
            stx VelX
xchecked:   stx NewX            ; NewX is OldX + VelX but VelX got zeroed if it would leave the map
            ldx OldY
            txa
            jsr setmapptr       ; locate original y-coordinate on map
            lda MapPtrL         ; and store in ZPtrB
            sta ZPtrB
            lda MapPtrH
            sta ZPtrB + 1       ; X-Byte was set up top
            lda VelY
            beq ychecked        ; if not moving vertically, need not check
            bmi yup             ; if moving up, jump to upward check
            inx
            bne ychecked        ; check to see if moving down wraps around the map
            stx VelY            ; ran off the bottom, stop vertical motion
            dex
            jmp ychecked
yup:        dex
            cpx #$FF            ; check to see if moving up wraps around the map
            bne ychecked
            inx                 ; ran off the top, stop vertical motion
            stx VelY
ychecked:   stx NewY            ; NewY is OldY + VelY but VelY got zeroed if it would leave the map
            txa
            jsr setmapptr       ; locate new y-coordinate on map
            lda MapPtrL         ; and store in ZPtrA
            sta ZPtrA
            lda MapPtrH
            sta ZPtrA + 1       ; X-Byte was set up top
            ldy NewX
            lda (ZPtrA), y      ; look at NewX, NewY (where we are trying to move)
            sta ZMapTemp        ; save for later in case we ran into a disk
            and #$3F            ; color bits don't block movement
            beq moveclear       ; clear to move, nothing in the way
            cmp #C_DISK         ; disk is the only non-obstacle
            beq diagdisk        ; moved atop a disk
            ; we have hit something trying to move in the intended direction
            lda VelX            ; see if we were attempting to move horizontally
            beq :+              ; if not, skip horizontal check
            lda (ZPtrB), y      ; check NewX, OldY (y still holds NewX)
            sta ZMapTemp        ; save for later in case we ran into a disk
            and #$3F            ; color bits don't block movement
            beq horizok         ; block was only vertical, we can move in the horiz
            cmp #C_DISK         ; disk is the only non-obstacle
            beq horizdisk       ; horizontal move lands on a disk
:           lda VelY            ; see if we were attempting to move vertically
            beq :+              ; if not, skip vertical check
            ldy OldX            ; check OldX, NewY
            lda (ZPtrA), y
            sta ZMapTemp        ; save for later in case we ran into a disk
            and #$3F            ; color bits don't block movement
            beq vertok          ; block was only horizontal, we can move in the vert
            cmp #C_DISK         ; disk is the only non-obstacle
            beq vertdisk        ; vertical move lands on a disk
            ; we have been stopped, move cannot be accomplished
:           sty NewX            ; y still holds OldX, cancel X movement
            lda OldY
            sta NewY            ; cancel Y movement
            lda #$00            ; set velocity to zero
            sta VelX
            sta VelY
            lda ZPtrB           ; make map line pointer for new Y be same as old Y
            sta ZPtrA
            lda ZPtrB + 1
            sta ZPtrA + 1
            sec                 ; move failed, return with carry set
            rts
horizdisk:  jsr gotdisk
horizok:    lda #$00            ; stop vertical movement
            sta VelY
            lda OldY            ; new hero Y is unchanged from old hero Y
            sta NewY
            lda ZPtrB           ; make map line pointer for new Y be same as old Y
            sta ZPtrA
            lda ZPtrB + 1
            sta ZPtrA + 1
            jmp moveclear
vertdisk:   jsr gotdisk
vertok:     lda #$00            ; stop horizontal movement
            sta VelX
            lda OldX            ; new hero X is unchanged
            sta NewX
            jmp moveclear
diagdisk:   jsr gotdisk
moveclear:  clc                 ; return with clear carry if move succeeds
            rts

; called if moving player/antagonist lands on a disk
gotdisk:    lda ZMapTemp        ; map (disk) was stored here, includes type
            and #$C0            ; determine disk type (color bits)
            pha
            ldx IsHero
            beq gotnotsnd
            ora #$20            ; add to score if hero got the disk
            lsr
            jsr addscore        ; add type multiplier to the score
            lda #$00
            sta ZFXPtr
            lda #$1F            ; play SndHeroGot sound ("hero got disk")
            sta ZFXPtr + 1
            sta ZFXPlay
            jmp gotaccount
gotnotsnd:  lda #$00
            sta ZFXPtr
            lda #$1E            ; play SndHrdrGot sound ("hoarder got disk")
            sta ZFXPtr + 1
            sta ZFXPlay
gotaccount: pla                 ; retrieve disk type (bits 7 and 8)
            asl                 ; shift them over to bits 1 and 2
            rol
            rol
            tax                 ; move type to x
            sed                 ; add in decimal mode to accounting for types
            lda IsHero
            beq gotnotact       ; only add to "got" if hero got it
            lda DisksGot, x
            clc
            adc #$01
            sta DisksGot, x     ; got one of this type
            lda #$50            ; start a splash
            sta SplashG, x
gotnotact:  lda DisksLeft, x
            sec
            sbc #$01
            sta DisksLeft, x    ; fewer out there of this type
            lda #$50            ; start a splash
            sta SplashL, x
            cld
            ; removing the disk is unnecessary because the hero/hoarder will replace it
            rts

; drop a disk of type in X
dropdisk:   lda DisksGot, x
            beq havenone
            lda #$00
            sta ZFXPtr
            lda #$1C            ; play the drop sound
            sta ZFXPtr + 1
            sta ZFXPlay
            ; TODO - actually drop it
            sed
            lda DisksGot, x
            sec
            sbc #$01
            sta DisksGot, x
            lda DisksLeft, x
            clc
            adc #$01
            sta DisksLeft, x
            cld
            rts
havenone:   lda #$00
            sta ZFXPtr
            lda #$1D            ; play the error sound ("d'oh!")
            sta ZFXPtr + 1
            sta ZFXPlay
            rts
