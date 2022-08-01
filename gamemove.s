; DISKHERO
; movement processing
;
; includes antagonist movement

domove:     lda #$82                ; all map-related stuff is in bank 2
            sta ZNewPtr + XByte     ; set up the XByte for all three pointers up here
            sta ZOldPtr + XByte     ; so we do not wind up setting them repeatedly
            sta ZTailPtr + XByte    ; inside a loop
            ; move everyone else before moving hero
            lda #$00
            sta ZIsHero         ; not hero - determines sound and score effects of hitting a disk
            ldy NumHoards
            sty ZCurrHoard
            ; move each hoarder in turn
            ; empty the bins of dirty segments
            ldx #$08
            lda #$00
:           sta ZDirtStack, x
            dex
            bpl :-
movehoard:  ldy ZCurrHoard
            lda (ZHoardTick), y ; ready to move yet? (up to 8 tick delay for each, governing speed)
            lsr                 ; countdown is just rotating out bits until none are left.
            beq ticksdone
            sta (ZHoardTick), y ; not ready to move, decrease ticks and move to check next hoarder
            jmp nexthoard
ticksdone:  lda (ZHoardSp), y   ; this one can move now, reset ticks for next move after this one
            sta (ZHoardTick), y
            lda (ZHoardY), y    ; load up the current hoarder's state (segment coords, velocity)
            sta ZOldY
            lda (ZHoardX), y
            sta ZOldX
            lda (ZHoardXV), y
            sta ZVelX
            lda (ZHoardYV), y
            sta ZVelY
            lda (ZHoardXX), y
            sta ZOldXX
            lda (ZHoardYY), y
            sta ZOldYY
            lda #$00            ; hoarders seek high-value
            jsr scandisks       ; get the closest highest value disk to ZOldX/Y in ZTargX/Y and Z
            bcs hnewton         ; branch to just continue in motion if no target was found
            ; there is a target
            ; hoarder will preferentially head toward it if it is exactly on target's X or Y
            ; but retains a chance of moving a different way so it does not get stuck forever by a wall
            ldx Seed            ; random chance that a hoarder will go a random direction
            inc Seed
            lda Random, x
            and #$0F            ; one in 16 chance that the hoader changes to a random direction
            beq hranddir
            ; if the hoarder is in the same row or column as target, head toward it
hoardseek:  lda ZOldX
            cmp ZTargX
            beq hgovert
            lda ZOldY
            cmp ZTargY
            beq hgohoriz
            ; if it is not a straight line to the target, then prefer to keep moving the same
            ; direction until stopped, and re-evaluate then
hnewton:    lda ZVelY
            beq hnovely         ; is hoarder stopped? Y Velocity has not answered this.
            jmp hmoving         ; if there was Y velocity, hoarder is moving
hnovely:    lda ZVelX           ; Y velocity was 0, is X velocity 0 also?
            bne hmoving         ; if there is X velocity, hoarder is moving
            ; when stopped, head toward the target on one of the available dimensions.
            lda ZTargD          ; check to see if we found a target disk to seek
            cmp #$FF
            beq hranddir        ; there is no target, so random direction it is
            ldx Seed            ; hoarder was not moving. so send it in a new direction
            inc Seed
            lda Random, x
            and #$07            ; one out of 8 chance it goes in a random direction
            beq hranddir        ; otherwise seeks high value disk
hoardtarg:  ldx Seed            ; randomly pick which axis to head for the disk in
            inc Seed
            lda Random, x
            bmi hgohoriz
hgovert:    lda ZTargDY         ; send it vertically toward the target
            sta ZVelY
            lda #$00
            sta ZVelX
            jmp hmoving
hgohoriz:   lda ZTargDX         ; send it horizontally toward the target
            sta ZVelX
            lda #$00
            sta ZVelY
            jmp hmoving
hranddir:   ldx Seed
            lda Random, x       ; send it in a random direction.  Which axis?
            bpl randhoriz       ; horizontal/vertical choice yields: horizontal
            inx
            lda Random, x       ; send it vertically in a random direction
            stx Seed
            sta ZVelY           ; will be either positive or negative (or zero)
            lda #$00
            sta ZVelX           ; do not move in the horizontal
            jmp randmoving
randhoriz:  inx
            lda Random, x       ; send it horizontally in a random direction
            stx Seed
            sta ZVelX           ; will be either positive or negative (or zero)
            lda #$00
            sta ZVelY           ; do not move in the vertical
            ; if we are changing to a random direction, swap head and hands
            ; so the hoarder does not corner itself
randmoving: lda ZOldXX          ; recall original second segment (head) X-coordinate
            pha                 ; stash it as we swap
            lda ZOldX           ; get the old hands x-coordinate
            sta ZOldXX          ; and put it in old head x-coordinate
            pla                 ; recall old head X-coordinate
            sta ZOldX           ; and put it in old hands x-coordinate
            lda ZOldYY          ; then do the same for the Y coordinates
            pha
            lda ZOldY
            sta ZOldYY
            pla
            sta ZOldY
hmoving:    jsr trymove         ; attempt to follow the trajectory
            bcc hmoved          ; the move succeded
            ldy ZCurrHoard      ; the move failed, zero out the velocity
            lda #$00            ; (will be sent in another direction next cycle)
            sta (ZHoardXV), y
            sta (ZHoardYV), y
            jmp nexthoard
hmoved:     ldy ZCurrHoard      ; move succeeded
            lda (ZHoardAnim), y ; toggle animation frames
            eor #$01
            sta (ZHoardAnim), y
            sta ZFrame          ; stash animation frame in ZFrame for use later
            lda ZOldYY          ; locate original second segment y-coordinate on map
            jsr setmapptr       ; locate original second segment y-coordinate on map
            lda MapPtrL
            sta ZTailPtr
            lda MapPtrH
            sta ZTailPtr + 1    ; X-Byte was set up top
            lda #$00            ; remove second segment from the map
            ldy ZOldXX
            sta (ZTailPtr), y
            lda #C_HHEADA       ; put head in old first segment (hands) position
            clc
            adc ZFrame          ; select animation frame
            ldy ZOldX
            sta (ZOldPtr), y
            ldy ZCurrHoard      ; update hoarder info
            lda ZNewX
            sta (ZHoardX), y
            lda ZNewY
            sta (ZHoardY), y
            lda ZVelX
            sta (ZHoardXV), y
            lda ZVelY
            sta (ZHoardYV), y
            lda ZOldX           ; copy prior location of the hands to
            sta (ZHoardXX), y   ; the location of the head
            lda ZOldY
            sta (ZHoardYY), y
            cmp ZNewY           ; and then determine which directions hands should go
            bcc handsdown       ; OldY (head) is less than NewY (hands), so point down
            lda ZOldX
            cmp ZNewX
            bcc handsright      ; OldX (head) is less than NewX (hands), so point right
            beq handsup         ; if OldX = NewX, and OldY is not less than NewY, point up
            lda #C_HHANDLA      ; otherwise head is more than hands, so point left.
            bne handoff
handsup:    lda #C_HHANDUA
            bne handoff
handsright: lda #C_HHANDRA
            bne handoff
handsdown:  lda #C_HHANDDA
handoff:    ldy ZNewX
            clc
            adc ZFrame          ; select animation frame
            sta (ZNewPtr), y    ; update the map
            jsr gmqupdate       ; update the screen
nexthoard:  dec ZCurrHoard
            bmi donehoard
            jmp movehoard
donehoard:  jsr hrcleanup       ; redraw the dirty segments in the hires screen
            ; now move hero
            sta ZIsHero         ; nonzero for hero - sets sound/score effects of hitting a disk
            lda HeroX           ; current position of Hero is OldX/OldY
            sta ZOldX
            lda HeroY
            sta ZOldY
            lda VelocityX       ; current velocity of Hero is VelX/VelY
            sta ZVelX
            lda VelocityY
            sta ZVelY
            jsr trymove         ; attempt to follow trajectory
            bcs posthero        ; the move failed, do not need to do a map update
            lda #$00            ; remove old hero from map
            ldy ZOldX
            sta (ZOldPtr), y
            lda #C_HERO         ; put new hero on map
            ldy ZNewY
            sty HeroY           ; record new Y location
            ldy ZNewX
            sty HeroX           ; record new X location
            sta (ZNewPtr), y    ; put hero in new place on the map
            ; would update on screen here except hero is never on the hires screen
posthero:   lda ZVelX
            sta VelocityX       ; record new X velocity (might have been stopped by a wall)
            lda ZVelY
            sta VelocityY       ; record new Y velocity (might have been stopped by a wall)
            ; find closest of each value of disk for display
            ; (and, honestly, also to help debug my searching algorithm, but may help game play too)
            ldx #$03
heroseek:   txa
            ora #$80
            jsr scandisks       ; look for target disk just of the value currently benig checked
            bcc heroseeky       ; success, record it
            lda #$80
            sta TargDX, x       ; record failure as 80 in the X direction.
            jmp heroseekn
heroseeky:  lda ZTargDX
            sta TargDX, x
            lda ZTargDY
            sta TargDY, x
heroseekn:  dex
            bpl heroseek
            lda #$01            ; mark playfield as in need of redraw
            sta PlayDirty
            lda VelocityY       ; did we need to scroll up based on hero movement?
            beq noscroll        ; nope
            bmi scrolldn
            clc                 ; yep, set scrolling direction parameter to "down" (clc)
            jmp scrollmap       ; scroll the screen (using smooth scroll) - rts from there
scrolldn:   sec
            jmp scrollmap       ; scroll the screen (using smooth scroll) - rts from there
noscroll:   rts

; queue potential changes to the list of segments that need to be updated on screen
gmqupdate:  ldy ZOldYY      ; where the head was (segment 2)
            ldx ZOldXX
            jsr hrdirty
            ldy ZOldY       ; where the hands were
            ldx ZOldX
            jsr hrdirty
            ldy ZNewY       ; where the hands are
            ldx ZNewX
            jmp hrdirty     ; rts from there

; scan disks for the most valuable closest one to ZOldX, ZOldY
; enter with A being $8x to search only for value x, else $00 to find most hoarder-valuable
scandisks:  sta ZTargV
            lda #$FF
            sta ZTargD          ; smallest distance found so far is 255.
            ldy NumDisks
dcheckdisk: lda (ZDiskX), y
            bmi dtoofar         ; a disk that's been taken off the board is not interesting
            sec
            sbc ZOldX           ; how X-far is this disk? DiskX - X: Negative if disk is to the left
            sta ZTargDXTemp     ; this is signed X vector from X to DiskX
            bcs dxdpos          ; branch away if already positive
            eor #$FF            ; else take absolute value for distance
            adc #$01
dxdpos:     sta ZTargDTemp      ; save X distance (scalar) for combining later
            lda (ZDiskY), y
            sec
            sbc ZOldY           ; how Y-far is this disk? DiskY - Y: Negative if disk is above
            bcc dyup            ; subtraction crossed zero, so disk above
            bmi dtoofar         ; disk is below but distance is negative (>$7F), too far to consider
            sta ZTargDYTemp     ; this is signed Y vector from Y to DiskY (positive, disk is down)
            jmp dcombdist
dyup:       bpl dtoofar         ; disk is above but distance is positive so must be >$7F away
            sta ZTargDYTemp     ; this is signed Y vector from Y to DiskY (negative, disk is up)
            eor #$FF            ; take absolute value for distance
            adc #$01
dcombdist:  clc                 ; add Y distance we just computed
            adc ZTargDTemp      ; with X distance from earlier, to get path distance
            sta ZTargDTemp      ; replace with path distance, this is now the total distance
            lda ZTargV          ; consider value of the current target
            bpl dfindval        ; branch if we are looking for most valuable (hoarder)
            and #$7F            ; otherwise we are looking for exactly one value
            cmp (ZDiskType), y  ; check disk value to see if it is the one we are looking for
            bne dtoofar         ; wrong value, we no longer find this disk interesting
            jmp dcheckdist      ; right value, so now compare distance
dfindval:   cmp (ZDiskType), y  ; check value (higher type = higher value)
            beq dcheckdist      ; if is the same as best value we've seen, check distance
            bcs dtoofar         ; if value we had before is higher, we no longer care about this disk
            lda (ZDiskType), y  ; this value is higher than we had before
            sta ZTargV          ; remember it as our new target value
            jmp dnewtarget      ; skip distance check, doesn't matter, this is the new target
dcheckdist: lda ZTargDTemp      ; compare this disk's distance to the best we've seen so far
            cmp ZTargD          ; is this closer than the last one we've seen at this value?
            bcs dtoofar         ; branch away if not strictly closer
dnewtarget: lda (ZDiskX), y     ; this is the new target
            sta ZTargX
            lda (ZDiskY), y
            sta ZTargY
            lda ZTargDTemp
            sta ZTargD
            lda ZTargDXTemp     ; direction toward the disk
            sta ZTargDX
            lda ZTargDYTemp     ; direction toward the disk
            sta ZTargDY
dtoofar:    dey
            bpl dcheckdisk
            ; at this point the target should contain the closest highest-value disk
            lda ZTargD
            cmp #$FF            ; sets carry if we didn't actually find one
            rts
            
; do moving (called maximally once per MoveDelay VBLs)
; (since otherwise it can be too fast, though speeding up might be a way to make the game harder)

; determine where we would move
; check for collision in x alone, y alone, x+y
; if x+y collides with obstacle, but x does not, stop y move x
; otherwise if y does not collide, stop x move y
; otherwise stop (prefers horizontal momentum)
; then actually do the move, if successful

trymove:    ldx ZOldX           ; first check whether move would take the object off the map
            lda ZVelX
            beq xchecked        ; if not moving horizontally, need not check
            bmi xleft           ; if moving left, jump to leftward check
            inx
            cpx #$3F            ; check to see if moving right goes off the map
            bne xchecked
            dex                 ; ran off the right edge, stop horizontal motion
            lda #$00
            sta ZVelX
            jmp xchecked
xleft:      dex                 ; check to see if moving left goes off the map
            bpl xchecked
            inx                 ; ran off the left edge, stop horizontal motion
            stx ZVelX
xchecked:   stx ZNewX           ; NewX is OldX + VelX but VelX got zeroed if it would leave the map
            ldx ZOldY
            txa
            jsr setmapptr       ; locate original y-coordinate on map
            lda MapPtrL         ; and store in ZOldPtr
            sta ZOldPtr
            lda MapPtrH
            sta ZOldPtr + 1     ; X-Byte was set up top in calling function
            lda ZVelY
            beq ychecked        ; if not moving vertically, need not check
            bmi yup             ; if moving up, jump to upward check
            inx
            bne ychecked        ; check to see if moving down wraps around the map
            stx ZVelY           ; ran off the bottom, stop vertical motion
            dex
            jmp ychecked
yup:        dex
            cpx #$FF            ; check to see if moving up wraps around the map
            bne ychecked
            inx                 ; ran off the top, stop vertical motion
            stx ZVelY
ychecked:   stx ZNewY           ; NewY is OldY + VelY but VelY got zeroed if it would leave the map
            txa
            jsr setmapptr       ; locate new y-coordinate on map
            lda MapPtrL         ; and store in ZNewPtr
            sta ZNewPtr
            lda MapPtrH
            sta ZNewPtr + 1     ; X-Byte was set up top
            ldy ZNewX
            ; at this point ZOldPtr and ZNewPtr are set and map boundary check is done
            lda (ZNewPtr), y    ; look at NewX, NewY (where we are trying to move)
            jsr checkcoll       ; what if anything would we hit? (y-coord in X, x-coord in Y)
            bcc moveok          ; we would hit nothing, so move
            ; obstacle in our intended path, check if just horizontal would work
            lda ZVelX           ; see if we were attempting to move horizontally
            beq skiphoriz       ; if not, skip horizontal check
            lda (ZOldPtr), y    ; check NewX, OldY (y still holds NewX)
            ldx ZOldY           ; load y-coord for checkcoll
            jsr checkcoll       ; what if anything would we hit? (y-coord in X, x-coord in Y)
            bcc horizok         ; we would hit nothing horizontally, so move horizontally
            ; check if just vertical would work
skiphoriz:  lda ZVelY           ; see if we were attempting to move vertically
            beq skipvert        ; if not, skip vertical check
            ldy ZOldX           ; check OldX, NewY
            lda (ZNewPtr), y
            ldx ZNewY           ; load y-coord for checkcoll
            jsr checkcoll       ; what if anything would we hit? (y-coord in X, x-coord in Y)
            bcc vertok          ; we would hit nothing vertically, so move vertically
            ; we have been stopped, move cannot be accomplished
skipvert:   ldx ZOldX           ; cancel X movement
            sty ZNewX
            lda ZOldY           ; cancel Y movement
            sta ZNewY
            lda #$00            ; set velocity to zero
            sta ZVelX
            sta ZVelY
            lda ZOldPtr         ; make map line pointer for new Y be same as old Y
            sta ZNewPtr
            lda ZOldPtr + 1
            sta ZNewPtr + 1
            sec                 ; move failed, return with carry set
            rts
horizok:    lda #$00            ; stop vertical movement
            sta ZVelY
            lda ZOldY           ; new Y is unchanged from old Y
            sta ZNewY
            lda ZOldPtr         ; make map line pointer for new Y be same as old Y
            sta ZNewPtr
            lda ZOldPtr + 1
            sta ZNewPtr + 1
            jmp moveok
vertok:     lda #$00            ; stop horizontal movement
            sta ZVelX
            lda ZOldX           ; new X is unchanged from old X
            sta ZNewX
moveok:     clc                 ; return with clear carry if move succeeds
            rts

; check map byte in A for whether it is a collosion or not
; returns with carry set (blocked), or clear (path was clear or contained only a disk)
; it is presumed that if this succeeds, the move WILL happen, because the
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
; assumes x-coordinate is in Y and y-coordinate is in X
gotdisk:    lda ZMapTemp        ; map (disk) was stored here, includes type
            sty DADiskX
            stx DADiskY
            and #$C0            ; determine disk type (color bits)
            pha                 ; stash color bits
            ldx ZIsHero
            beq gotnotsnd       ; if it is not the hero moving, play hoarder sound
            eor #$C0            ; invert value for points (type 0 gets most, 3 least)
            ora #$20            ; add to score if hero got the disk
            lsr
            jsr addscore        ; add type multiplier to the score
            lda #$00
            sta ZFXPtr
            lda #SFXHeroGot     ; play "hero got disk" sound effect
            sta ZFXPtr + 1
            sta FXPlaying       ; start playing the sound
            jmp gotaccount
gotnotsnd:  lda #$00
            sta ZFXPtr
            lda #SFXHrdrGot     ; play "hoarder got disk" sound effect
            sta ZFXPtr + 1
            sta FXPlaying       ; start playing
gotaccount: pla                 ; retrieve disk type (bits 7 and 8)
            asl                 ; shift them over to bits 1 and 2
            rol
            rol
            tax                 ; move type to x
            sed                 ; add in decimal mode to accounting for types
            lda ZIsHero
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
            ; remove the disk from the inventory of disks
            ; we have to figure out which disk this was from its coordinates
            ldy NumDisks
gotfind:    lda (ZDiskX), y
DADiskX = *+1
            cmp #INLINEVAR
            bne gotnotthis
            lda (ZDiskY), y
DADiskY = *+1
            cmp #INLINEVAR
            beq gotthis
gotnotthis: dey
            bpl gotfind
            ; should never fall through to here, will corrupt memory (disk=FF) if it does
            ; take the disk off the board, record who has it.
gotthis:    lda ZIsHero
            beq gothoard        ; hoader has it
            lda #$FF            ; hero has it
            jmp gotremove
gothoard:   lda ZCurrHoard      ; keep track of which hoarder has it, for someday
            ora #$80            ; set the high bit
gotremove:  sta (ZDiskX), y     ; if X is negative, will have 80+hoarder or FF (hero) in it
            ; removing the disk from the map is unnecessary
            ; because the hero/hoarder will replace it
            rts

; drop a disk of type in X
dropdisk:   lda DisksGot, x
            bne drophave
            jmp dropfail        ; don't have any to drop
drophave:   stx ZPxScratch      ; stash type
            lda HeroY
            tax                 ; put y-coord in X for dodrop
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
            jmp dodrop          ; x still holds HeroY from before
dropup:     lda HeroY
            beq dropdown
            sec
            sbc #$01
            tax                 ; put y-coord in X for dodrop
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
            tax                 ; put y-coord in X for dodrop
            jsr setmapptr
            lda MapPtrL
            sta ZPtrA
            lda MapPtrH
            sta ZPtrA + 1
            ldy HeroX
            lda (ZPtrA), y
            bne dropfail
dodrop:     sty DroppedX        ; save x-coordinate
            stx DroppedY        ; save y-coordinate
            ldy NumDisks        ; find an open slot, there MUST be one
dropfind:   lda (ZDiskX), y
            bmi dropfound
            dey
            bpl dropfind
            ; should never fall through to here, we had one to drop
DroppedY = *+1
dropfound:  lda #INLINEVAR
            sta (ZDiskY), y
DroppedX = *+1
            lda #INLINEVAR
            sta (ZDiskX), y
            lda ZPxScratch      ; get type back
            sta (ZDiskType), y
            ror
            ror
            ror                 ; move type to high two bits
            pha                 ; push for use with score subtraction
            ora #C_DISK
            ldy DroppedX
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
            lda #SFXDrop        ; play the drop sound
            sta ZFXPtr + 1
            sta FXPlaying
            rts
dropfail:   lda #$00
            sta ZFXPtr
            lda #SFXDoh         ; play the drop error sound ("d'oh!")
            sta ZFXPtr + 1
            sta FXPlaying
            rts
