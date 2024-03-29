; DISKHERO
; map construction

; This is called early and fills in map and tracking data in bank 2.
; It does not switch the banks, and does not present any externally-accessible
; variables.  So it should be ok in bank switched memory.
;
; Bank 2:
; Map data is from $2000-5FFF.
; Element tracking variables are from $300-640.
; 
; the map is $40 units wide and $100 units tall.
; map bytes have shape info in lower 6 bits, color/variant info in higher 2.
; the last byte of every map line is unused, the playable lines are only $3F wide.
; because it needs to be a multiple of 7.

; This is still a bit of a placeholder.  In order to avoid spending time building
; levels or mazes, this makes a "maze-like" structure by scattering boxes around
; Also places the hero, the disks, and the hoarders.

; 5x5 box patterns for randomly adding to the map.  Each pattern is $19 bytes long.
BoxIndex:   .byte 0, 25, 50, 75, 100, 125, 150, 175
BoxPatt:
            ; box open leftward
            .byte   C_WALL_R,   C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_LD
            .byte   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_V
            .byte   C_SPACE,    C_SPACE,    C_WALL_RD,  C_WALL_H,   C_WALL_LU
            .byte   C_SPACE,    C_SPACE,    C_WALL_V,   C_SPACE,    C_SPACE
            .byte   C_WALL_R,   C_WALL_H,   C_WALL_LU,  C_SPACE,    C_SPACE
            ; box open leftward
            .byte   C_WALL_R,   C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_LD
            .byte   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_V
            .byte   C_SPACE,    C_SPACE,    C_WALL_R,   C_WALL_H,   C_WALL_LUD
            .byte   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_V
            .byte   C_WALL_R,   C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_LU
            ; box open rightward
            .byte   C_WALL_RD,  C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_L
            .byte   C_WALL_V,   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE
            .byte   C_WALL_RUD, C_WALL_H,   C_WALL_L,   C_SPACE,    C_SPACE
            .byte   C_WALL_V,   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE
            .byte   C_WALL_RU,  C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_L
            ; box open upward
            .byte   C_WALL_D,   C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_D
            .byte   C_WALL_V,   C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_V
            .byte   C_WALL_V,   C_SPACE,    C_WALL_D,   C_SPACE,    C_WALL_V
            .byte   C_WALL_V,   C_SPACE,    C_WALL_V,   C_SPACE,    C_WALL_V
            .byte   C_WALL_RU,  C_WALL_H,   C_WALL_LRU, C_WALL_H,   C_WALL_LU
            ; box open downward
            .byte   C_WALL_RD,  C_WALL_H,   C_WALL_LRD, C_WALL_H,   C_WALL_LD
            .byte   C_WALL_V,   C_SPACE,    C_WALL_V,   C_SPACE,    C_WALL_V
            .byte   C_WALL_V,   C_SPACE,    C_WALL_U,   C_SPACE,    C_WALL_V
            .byte   C_WALL_V,   C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_V
            .byte   C_WALL_U,   C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_U
            ; well open left and right
            .byte   C_WALL_R,   C_WALL_H,   C_WALL_LRD, C_WALL_H,   C_WALL_L
            .byte   C_SPACE,    C_SPACE,    C_WALL_V,   C_SPACE,    C_SPACE
            .byte   C_SPACE,    C_SPACE,    C_WALL_V,   C_SPACE,    C_SPACE
            .byte   C_SPACE,    C_SPACE,    C_WALL_V,   C_SPACE,    C_SPACE
            .byte   C_WALL_R,   C_WALL_H,   C_WALL_LRU, C_WALL_H,   C_WALL_L
            ; box zigzag
            .byte   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_D
            .byte   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_V
            .byte   C_WALL_RD,  C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_LU
            .byte   C_WALL_V,   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE
            .byte   C_WALL_U,   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE
            ; box open down and right
            .byte   C_WALL_R,   C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_LD
            .byte   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_V
            .byte   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_V
            .byte   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_V
            .byte   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE,    C_WALL_U
            ; box open down and left
            .byte   C_WALL_RD,  C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_L
            .byte   C_WALL_D,   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE
            .byte   C_WALL_D,   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE
            .byte   C_WALL_D,   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE
            .byte   C_WALL_D,   C_SPACE,    C_SPACE,    C_SPACE,    C_SPACE
MFX:        .byte 0
MFY:        .byte 0
MFColor:    .byte 0
MFBoxIndex: .byte 0
MFPlaced:   .byte 0
MFDiskType: .byte 0
    
mapinit:   
            ; Start by initializing memory pointers for location tracking.
            ; All of this is in bank 2.
            ; DiskX = 300, DiskY = 340, DiskType = 380
            ; HoardX = 400, HoardY = 440, HoardXV = 480,
            ; HoardYV = 4C0, HoardSp = 500, HoardXX = 540, 
            ; HoardYY = 580, HoardTick = 5C0,
            ; HoardAnim = 600
            lda #$00
            sta ZDiskX
            sta ZHoardX
            sta ZHoardSp
            sta ZHoardAnim
            lda #$40
            sta ZDiskY
            sta ZHoardY
            sta ZHoardXX
            lda #$80
            sta ZDiskType
            sta ZHoardXV
            sta ZHoardYY
            lda #$C0
            sta ZHoardYV
            sta ZHoardTick
            lda #$03
            sta ZDiskX + 1
            sta ZDiskY + 1
            sta ZDiskType + 1
            lda #$04
            sta ZHoardX + 1
            sta ZHoardY + 1
            sta ZHoardXV + 1
            sta ZHoardYV + 1
            lda #$05
            sta ZHoardSp + 1
            sta ZHoardXX + 1
            sta ZHoardYY + 1
            sta ZHoardTick + 1
            lda #$06
            sta ZHoardAnim + 1
            lda #$82
            sta ZDiskX + XByte
            sta ZDiskY + XByte
            sta ZDiskType + XByte
            sta ZHoardX + XByte
            sta ZHoardY + XByte
            sta ZHoardXV + XByte
            sta ZHoardYV + XByte
            sta ZHoardSp + XByte
            sta ZHoardXX + XByte
            sta ZHoardYY + XByte
            sta ZHoardTick + XByte
            sta ZHoardAnim + XByte
            ; now start building the map.
            ; Step 1: empty it.
            lda #$00
            sta ZPtrA
            lda #$20
            sta ZPtrA + 1
            lda #$82
            sta ZPtrA + XByte
            ; seed the "random" number list
            jsr seedRandom
            ; zero out the map background
            ldx #$40        ; clearing $40 pages
            ldy #$00
            lda #C_SPACE
mfzero:     sta (ZPtrA), y
            iny
            bne mfzero
            inc ZPtrA + 1
            dex
            bne mfzero
            ; place some boxes
            ; every 8x8 cell gets a 5x5 box "centered" in it
            ; since we're always going one down and one in, start at 1, 1
            lda #$01
            sta MFX
            sta MFY
mfbox:      ldx Seed
            lda Random, x   ; pick a random box shape
            and #$07
            tay
            lda BoxIndex, y
            sta MFBoxIndex
            inx
            lda Random, x   ; pick a random box color
            and #$C0
            sta MFColor
            inx
            stx Seed
            ; locate address in map for coordinate MFX, MFY
            lda #$00
            sta ZPtrA
            ; rotate low bits of MFY into high bits of ZPtrA(L)
            ; then add $2000 and store in ZPtrA(H)
            ; so: if map coordinate MFY were $21, shift it so we have
            ; MFY: 0010 0001 -> Zptr: 0100 0000 0001 1000 (40 18 -> $2840)
            ; which is effectively multiplying Y by $40 and adding $2000.
            ; Then, add MFX to get the horizontal offset
            ; ZPtrA will then point to the upper left corner of box target
            lda MFY
            lsr
            ror ZPtrA
            lsr
            ror ZPtrA
            clc
            adc #$20
            sta ZPtrA + 1
            lda ZPtrA
            clc
            adc MFX
            sta ZPtrA
            ; put a pattern row in the map
            ldy #$04
            sty MFPlaced        ; do 5 rows
mfpattrow:  lda MFBoxIndex      ; x points to the row of the box pattern
            clc
            adc #$04
            tax
:           lda BoxPatt, x
            ora MFColor
            sta (ZPtrA), y
            dex
            dey
            bpl :-
            ; done if we have completed 6 rows now
            dec MFPlaced
            bmi :+
            ; reset horizontal index to the end of the pattern
            ldy #$04
            ; move to next pattern row
            lda MFBoxIndex
            clc
            adc #$05
            sta MFBoxIndex
            ; move to the next map row
            lda ZPtrA
            clc
            adc #$40
            sta ZPtrA
            bcc mfpattrow
            inc ZPtrA + 1
            bcs mfpattrow
:
            lda MFX             ; move to next X coordinate
            cmp #$39            ; is this the last X coordinate on the line?
            beq :+
            clc
            adc #$08
            sta MFX
            jmp mfbox
:
            lda #$01            ; back to left side
            sta MFX
            lda MFY             ; move to next Y coordinate
            cmp #$F9            ; is the the last Y coordinate on the map?
            beq :+
            clc
            adc #$08
            sta MFY
            jmp mfbox
:           
            lda HeroY           ; drop the hero in before we scatter things
            jsr setmapptr
            lda MapPtrL
            sta ZPtrA
            lda MapPtrH
            sta ZPtrA + 1
            ldy HeroX
            lda #C_HERO
            sta (ZPtrA), y

            lda #$03            ; start with disk type 3 and move down
            sta MFDiskType
            lda #$00
            sta NumDisks
mfplacetyp: lda #$08            ; disks of each type to scatter around
            sta MFPlaced
mfplacedsk: jsr mfrndspot
            sty ZPxScratch      ; stash the x-coordinate
            ldy NumDisks
            txa                 ; store the y-coordinate
            sta (ZDiskY), y
            lda ZPxScratch      ; store the x-coordinate
            sta (ZDiskX), y
            lda MFDiskType      ; store the type
            sta (ZDiskType), y
            tax
            sed
            lda DisksLeft, x    ; record another one in the map inventory
            clc
            adc #$01
            sta DisksLeft, x
            cld
            lsr
            ror
            ror                 ; convert type to color bits
            ora #C_DISK         ; use disk shape
            ldy ZPxScratch      ; recall the x-coordinate
            sta (ZPtrA), y      ; place a disk
            inc NumDisks
            dec MFPlaced
            bne mfplacedsk
            dec MFDiskType
            bpl mfplacetyp
            
            lda #$10            ; hoarders to scatter around
            sta MFPlaced
            sta NumHoards
:           jsr mfrndspot
            sty ZPxScratch      ; stash the x-coordinate
            txa
            ldy MFPlaced
            ; for now all hoarders will face rightward initially
            sta (ZHoardYY), y
            sta (ZHoardY), y
            lda ZPxScratch
            sta (ZHoardXX), y   ; second segment (head) to the left
            clc
            adc #$01
            sta (ZHoardX), y    ; first segment (hands) to the right
            lda #$00
            sta (ZHoardAnim), y ; set animation frame to zero
            lda #$00
            sta (ZHoardXV), y
            lda #$00
            sta (ZHoardYV), y   ; velocity is zero so will go random direction next cycle
            ldx Seed            ; set speed (delay)
            lda Random, x       ; to a random number
            and #$0F            ; from 1 to 4 (ticks count LSRs until zero)
            inc Seed
            sta (ZHoardSp), y
            ldy ZPxScratch      ; place the hoarder
            lda #C_HHEADA
            sta (ZPtrA), y
            iny
            lda #C_HHANDRA
            sta (ZPtrA), y
            dec MFPlaced
            bpl :-

mfdone:     rts

; find a random spot that is open in the map
; (more specifically, two horizontally-adjacent free locations)
; returns with y holding the map x coordinate, x holding the map y coordinate,
; and ZPtrA pointing to the map row start
mfrndspot:  ldx Seed
            lda Random, x
            inx
            pha                     ; stash the y coordinate
            jsr setmapptr
rndx:       lda Random, x
            inx
            stx Seed
            and #$3F
            cmp #$3D                ; furthest right it can be is 3D since it needs two
            bcs rndx                ; if it was too far right, go try again.
            tay
            lda MapPtrL
            sta ZPtrA
            lda MapPtrH
            sta ZPtrA + 1
            lda #$82
            sta ZPtrA + XByte
            pla                     ; unstash the y coordinate in case we're done
            tax
            lda (ZPtrA), y
            bne mfrndspot           ; if spot wasn't empty keep looking maybe forever
            iny                     ; spot needs to have space to its right as well
            lda (ZPtrA), y
            bne mfrndspot           ; if spot wasn't empty keep looking maybe forever
            dey
            rts
