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
DoScrollUp: .byte   0
DoScrollDn: .byte   0
TargX:      .byte   0
TargY:      .byte   0
TargV:      .byte   0
TargD:      .byte   0
TargDX:     .byte   0
TargDY:     .byte   0
TargDTemp:  .byte   0

domove:     lda #$82                ; all map-related stuff is in bank 2
            sta ZNewPtr + XByte     ; set up the XByte for all three pointers up here
            sta ZOldPtr + XByte     ; so we do not wind up setting them repeatedly
            sta ZTailPtr + XByte    ; inside a loop
            ; move hero
            sta IsHero          ; determines sound and score effects of hitting a disk
            lda #$00            ; reset scroll up/down triggers
            sta DoScrollUp
            sta DoScrollDn
            lda HeroX           ; current position of Hero is OldX/OldY
            sta OldX
            lda HeroY
            sta OldY
            lda VelocityX       ; current velopcity of Hero is VelX/VelY
            sta VelX
            lda VelocityY
            sta VelY
            jsr trymove         ; attempt to follow trajectory
            bcs posthero        ; the move failed, do not need to do a map update
            lda #$00            ; remove old hero from map
            ldy OldX
            sta (ZOldPtr), y
            lda #C_HERO         ; put new hero on map
            ldy NewY
            sty HeroY           ; record new Y location
            ldy NewX
            sty HeroX           ; record new X location
            sta (ZNewPtr), y    ; put hero in new place on the map
            ; would update on screen here except hero is never on the hires screen
posthero:   lda VelX
            sta VelocityX       ; record new X velocity
            lda VelY
            sta VelocityY       ; record new Y velocity
            beq herodone        ; skip scrolling the map if there was no Y movement
            bmi scrolldown      ; if Y velocity is negative (hero moves up), map scrolls down
            inc DoScrollUp      ; if Y velocity is positive (hero moves down), map scrolls up
            jmp herodone
scrolldown: inc DoScrollDn
herodone:   lda #$00            ; hero finished, now move hoarders
            sta IsHero          ; determines sound and score effects of hitting a disk
            ldy NumHoards
            sty CurrHoard
movehoard:  ldy CurrHoard
            lda (ZHoardTick), y ; ready to move yet? (up to 8 tick delay for each, governing speed)
            lsr                 ; countdown is just rotating out bits until none are left.
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
            and #$0F            ; one in 16 chance hoarder stops (maybe changes directions)
            bne hoardcont
            lda #$00            ; horder stopped and may change direction.
            sta VelY
            sta VelX
            beq hoardredir
hoardcont:  lda (ZHoardXV), y
            sta VelX
            lda (ZHoardYV), y
            sta VelY
            beq hnovely         ; is hoarder stopped? Y Velocity has not answered this.
            jmp hmoving         ; if there was Y velocity, hoarder is moving
hnovely:    lda VelX            ; Y velocity was 0, is X velocity 0 also?
            beq hnotmoving      ; no X or Y velocity, so hoarder has stopped
            jmp hmoving         ; if there is X velocity, hoarder is moving
            ; swap head and hands if hoarder was stopped, so it does not corner itself
hnotmoving: lda ZXXTemp         ; recall original second segment (head) X-coordinate
            pha                 ; stash it as we swap
            lda (ZHoardX), y    ; get the old hands x-coordinate
            sta (ZHoardXX), y   ; and put it in old head x-coordinate
            sta ZXXTemp         ; which we also are continuing to save for later use
            pla                 ; recall old head X-coordinate
            sta (ZHoardX), y    ; and put it in old hands x-coordinate
            lda (ZHoardYY), y    ; then do the same for the Y coordinates
            pha
            lda (ZHoardY), y
            sta (ZHoardYY), y
            pla
            sta (ZHoardY), y
hoardredir: ldx Seed            ; hoarder was not moving. so send it in a new direction
            lda Random, x
            inx
            stx Seed
            and #$07            ; one out of 8 chance it goes in a random direction
            bne hoardseek       ; otherwise seeks high value disk
            jmp hranddir        ; hoader will go forth randomly
hoardseek:  lda #$FF            ; scan disks to find closest highest value one
            sta TargD           ; smallest distance found so far is 255.
            lda #$00            ; reset target X, Y, and value
            sta TargX
            sta TargY
            sta TargV
            ldy NumDisks
            sty CurrDisk
dcheckdist: ldy CurrDisk
            ;lda (ZDiskType), y  ; check value (higher type = higher value)
            ;cmp TargV           ; is this one more valuable than what we have seen?
            ;bcs dvaluable
            ;ldy CurrDisk        ; disk we found hasn't beat the value, but is it closer?
            lda (ZDiskX), y
            ldy CurrHoard
            sec
            sbc (ZHoardX), y
            sta TargDX          ; will be negative if disk is to the left
            bcs dxdpos
            eor #$FF            ; make it positive (take the absolute value)
            adc #$01
dxdpos:     sta TargDTemp       ; stash distance on the X-axis.
            ldy CurrDisk
            lda (ZDiskY), y
            ldy CurrHoard
            sec
            sbc (ZHoardY), y
            sta TargDY          ; will be negative if disk is above
            bcs dydpos
            eor #$FF            ; make it positive (take the absolute value)
            adc #$01
dydpos:     clc                 ; add X and Y distances.  Not very accurate.
            adc TargDTemp       ; also signed. so if disk is very far might appear close.
            cmp TargD           ; is the target we are checking closer than our prior target?
            bcs dnextdisk       ; this distance is greater than or equal to what we already found
            sta TargD           ; this disk is closer than other same value
            ldy CurrDisk
            lda (ZDiskX), y     ; so make this the new target
            sta TargX
            lda (ZDiskY), y            
            sta TargY
            lda TargDX          ; velocity is just pos/neg/zero anyway, so store
            sta VelX            ; 8-way direction to disk as the new hoarder velocity to try
            lda TargDY
            sta VelY
            jmp dnextdisk
dvaluable:  lda (ZDiskX), y     ; higher value than ones previously seen
            sta TargX           ; store this as the new target
            lda (ZDiskY), y     ; higher value than ones previously seen
            sta TargY
            lda TargDX          ; velocity is just pos/neg/zero anyway, so store
            sta VelX            ; 8-way direction to disk as the new hoarder velocity to try
            lda TargDY
            sta VelY
dnextdisk:  dec CurrDisk        ; move on to next disk.
            bpl dcheckdist
            lda VelX            ; do not let the hoarders go diagonally
            beq hmoving         ; if X velocity is zero, this one is not going diagonally
            lda VelY            ; if X velocity is nonzero, check if Y velocity is too
            beq hmoving         ; Y velocity is zero, it's moving horizontally
            ldx Seed            ; randomly pick which velocity to zero out
            lda Random, x
            bmi hgohoriz
            lda #$00
            sta VelX
            jmp hgoflat
hgohoriz:   lda #$00
            sta VelY
hgoflat:    inx
            stx Seed
            jmp hmoving         ; and go
hranddir:   lda Random, x       ; send it in a random direction.  Which axis?
            bpl sendhoriz       ; horizontal/vertical choice yields: horizontal
            inx
            lda Random, x       ; send it vertically in a random direction
            stx Seed
            sta VelY            ; will be either positive or negative (or zero)
            lda #$00
            sta VelX            ; do not move in the horizontal
            jmp hmoving
sendhoriz:  inx
            lda Random, x       ; send it horizontally in a random direction
            stx Seed
            sta VelX            ; will be tierh positive or negative (or zero)
            lda #$00
            sta VelY            ; do not move in the vertical
hmoving:    ldy CurrHoard       ; put hoarder's current position in OldY/OldX
            lda (ZHoardY), y
            sta OldY
            lda (ZHoardX), y
            sta OldX
            jsr trymove         ; and attempt to follow the trajectory
            bcc hmoved          ; the move succeded
            ldy CurrHoard       ; the move failed, zero out the velocity
            lda #$00            ; (will be sent in another direction next cycle)
            sta (ZHoardXV), y
            sta (ZHoardYV), y
            jmp nexthoard
hmoved:     ldy CurrHoard       ; move succeeded
            lda (ZHoardAnim), y ; toggle animation frames
            eor #$01
            sta (ZHoardAnim), y
            sta ZFrame          ; stash animation frame in ZFrame for use later
            lda (ZHoardYY), y   ; replace second segment (head) with zero
            sta OldOldY
            jsr setmapptr       ; locate original second segment y-coordinate on map
            ldy ZXXTemp         ; we stashed second segment X coordinate in here earlier
            sty OldOldX
            lda MapPtrL
            sta ZTailPtr
            lda MapPtrH
            sta ZTailPtr + 1    ; X-Byte was set up top
            lda #$00            ; remove second segment from the map
            sta (ZTailPtr), y
            lda #C_HHEADA       ; put head in old first segment (hands) position
            clc
            adc ZFrame          ; select animation frame
            ldy OldX
            sta (ZOldPtr), y
            ldy CurrHoard
            lda NewX
            sta (ZHoardX), y
            lda NewY
            sta (ZHoardY), y
            lda VelX
            sta (ZHoardXV), y
            lda VelY
            sta (ZHoardYV), y
            lda OldX            ; copy prior location of the hands to
            sta (ZHoardXX), y   ; the location of the head
            lda OldY
            sta (ZHoardYY), y
            cmp NewY            ; and then determine which directions hands should go
            bcc handsdown       ; OldY (head) is less than NewY (hands), so point down
            lda OldX
            cmp NewX
            bcc handsright      ; OldX (head) is less than NewX (hands), so point right
            beq handsup         ; if OldX = NewX, and OldY is not less than NewY, point up
            lda #C_HHANDLA      ; otherwise head is more than hands, so point left.
            bne handoff
handsup:    lda #C_HHANDUA
            bne handoff
handsright: lda #C_HHANDRA
            bne handoff
handsdown:  lda #C_HHANDDA
handoff:    ldy NewX
            clc
            adc ZFrame          ; select animation frame
            sta (ZNewPtr), y    ; update the map
            jsr gmupdscr        ; update the screen
nexthoard:  dec CurrHoard
            bmi donehoard
            jmp movehoard
donehoard:  jsr drawplay        ; redraw middle playfield
            lda DoScrollUp      ; did we need to scroll up based on hero movement?
            beq noscrollup      ; nope
            clc                 ; yep, set scrolling direction parameter to "down" (clc)
            jmp updatemap       ; scroll the screen (using smooth scroll) - rts from there
noscrollup: lda DoScrollDn      ; did we need to scroll down based on hero movement?
            beq noscrolldn      ; nope
            sec                 ; yep, set scrolling direction parameter to "up" (sec)
            jmp updatemap       ; scroll the screen (using smooth scroll) - rts from there
noscrolldn: rts

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
            jmp updsingle   ; rts from there

; do moving (called maximally once per MoveDelay VBLs)
; (since otherwise it can be too fast, though speeding up might be a way to make the game harder)

; determine where we would move
; check for collision in x alone, y alone, x+y
; if x+y collides with obstacle, but x does not, stop y move x
; otherwise if y does not collide, stop x move y
; otherwise stop (prefers horizontal momentum)
; then actually do the move, if successful

NewHeroX:   .byte   0
NewHeroY:   .byte   0

trymove:    ldx OldX            ; first check whether move would take the object off the map
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
            lda MapPtrL         ; and store in ZOldPtr
            sta ZOldPtr
            lda MapPtrH
            sta ZOldPtr + 1     ; X-Byte was set up top in calling function
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
            lda MapPtrL         ; and store in ZNewPtr
            sta ZNewPtr
            lda MapPtrH
            sta ZNewPtr + 1       ; X-Byte was set up top
            ldy NewX
            lda (ZNewPtr), y    ; look at NewX, NewY (where we are trying to move)
            jsr checkcoll       ; what if anything did we hit?
            bcc moveok          ; hit nothing, move
            ; obstacle in our intended path, check if just horizontal would work
            lda VelX            ; see if we were attempting to move horizontally
            beq skiphoriz       ; if not, skip horizontal check
            lda (ZOldPtr), y    ; check NewX, OldY (y still holds NewX)
            jsr checkcoll       ; what if anything did we hit?
            bcc horizok         ; hit nothing horizontally, move horizontally
            ; check if just vertical would work
skiphoriz:  lda VelY            ; see if we were attempting to move vertically
            beq skipvert        ; if not, skip vertical check
            ldy OldX            ; check OldX, NewY
            lda (ZNewPtr), y
            jsr checkcoll
            bcc vertok          ; hit nothing vertically, move vertically
            ; we have been stopped, move cannot be accomplished
skipvert:   ldx OldX            ; cancel X movement
            sty NewX
            lda OldY
            sta NewY            ; cancel Y movement
            lda #$00            ; set velocity to zero
            sta VelX
            sta VelY
            lda ZOldPtr         ; make map line pointer for new Y be same as old Y
            sta ZNewPtr
            lda ZOldPtr + 1
            sta ZNewPtr + 1
            sec                 ; move failed, return with carry set
            rts
horizok:    lda #$00            ; stop vertical movement
            sta VelY
            lda OldY            ; new hero Y is unchanged from old hero Y
            sta NewY
            lda ZOldPtr         ; make map line pointer for new Y be same as old Y
            sta ZNewPtr
            lda ZOldPtr + 1
            sta ZNewPtr + 1
            jmp moveok
vertok:     lda #$00            ; stop horizontal movement
            sta VelX
            lda OldX            ; new hero X is unchanged
            sta NewX
moveok:     clc                 ; return with clear carry if move succeeds
            rts

; check map byte in A for whether it is a collosion or not
; returns with carry set (blocked), or clear (path was clear or contained only a disk)
; disk gets marked as collected and sound is triggered in this process
checkcoll:  sta ZMapTemp        ; save for later in case we ran into a disk and need to test type
            clc
            and #$3F            ; color bits don't block movement, strip them away
            beq moveclear       ; clear to move, nothing in the way
            cmp #C_DISK         ; disk is the only non-obstacle
            bne mvblocked
            jsr gotdisk         ; do the disk accounting
moveclear:  clc                 ; carry clear = move succeeded (possibly collected a disk)
            rts
mvblocked:  sec                 ; carry set = move blocked
            rts

; called if moving player/antagonist lands on a disk
gotdisk:    lda ZMapTemp        ; map (disk) was stored here, includes type
            and #$C0            ; determine disk type (color bits)
            pha
            ldx IsHero
            beq gotnotsnd
            eor #$C0            ; invert value for points (type 0 gets most, 3 least)
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
            bne drophave
            jmp dropfail        ; don't have any to drop
drophave:   stx ZPxScratch      ; stash type
            lda HeroY
            jsr setmapptr
            lda MapPtrL
            sta ZPtrA
            lda MapPtrH
            sta ZPtrA + 1
            lda #$82
            sta ZPtrA + XByte
            ldy HeroX
            beq dropright       ; at the left edge, cannot drop left
            dey
            lda (ZPtrA), y      ; is left space empty?
            bne dropright
            jmp dodrop
dropright:  ldy HeroX           ; left wasn't empty, try right instead
            iny
            cpy #$3F
            beq dropup          ; left did not work and right is off the map
            lda (ZPtrA), y      ; is right space empty?
            bne dropup
            jmp dodrop
dropup:     lda HeroY
            beq dropdown
            sec
            sbc #$01
            jsr setmapptr
            lda MapPtrL
            sta ZPtrA
            lda MapPtrH
            sta ZPtrA + 1
            ldy HeroX
            lda (ZPtrA), y
            bne dropdown
            jmp dodrop
dropdown:   lda HeroY           ; left, right, up all failed, try down
            clc
            adc #$01
            beq dropfail        ; down failed too
            jsr setmapptr
            lda MapPtrL
            sta ZPtrA
            lda MapPtrH
            sta ZPtrA + 1
            ldy HeroX
            lda (ZPtrA), y
            bne dropfail
dodrop:     lda ZPxScratch      ; get type back
            ror
            ror
            ror                 ; move type to high two bits
            pha                 ; push for use with score subtraction
            ora #C_DISK
            sta (ZPtrA), y      ; and drop it
            pla                 ; get high bit type back
            eor #$C0            ; invert value for points (type 0 gets most, 3 least)
            ora #$20
            lsr
            jsr subscore        ; subtract type multiplier from the score
            ldx ZPxScratch      ; get type back
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
            lda #$00
            sta ZFXPtr
            lda #$1C            ; play the drop sound
            sta ZFXPtr + 1
            sta ZFXPlay
            rts
dropfail:   lda #$00
            sta ZFXPtr
            lda #$1D            ; play the error sound ("d'oh!")
            sta ZFXPtr + 1
            sta ZFXPlay
            rts
