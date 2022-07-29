; DISKHERO
; sound effect initialization

; This is called early and fills in static sound data in bank 1.
; It does not switch the banks, and does not present any externally-accessible
; variables.  So it should be ok in bank switched memory.

; Sound effects live in the lower pages, and are copied in by these routines.
; There is room for 29 sound effects, that should be good enough.
; The player does not handle sound effects with more than $100 samples.
; And probably best to steer clear of the lower $300 in the bank.
; One sound effect can be played at a time, and it takes priority over the
; background music.  The way it is set up, a new sound effect triggered will
; immediately supersede any that might currently be playing.
;
; 1F00-1FFF: sound effect 1F
; 1E00-1EFF: sound effect 1E
; ...
; 0300-03FF: sound effect 03
;
; background music segments live in upper part of the bank.
; The code at present generates them and copies them in.  A background sound
; can be arbitrarily long, so long as it fits in the bank.  We're have $6000
; available for these.  That seems to be about 24 seconds' worth if we used it
; all.  Planning to add a sound generator that will keep creating the soundtrack
; dynamically in this space instead, but the technology is the same.
;
; 2000-3FFF: background segment 1
; 4000-5FFF: background segment 2
; 6000-7FFF: background segment 3
;
; Sounds (both effects and background segments) are finished when they hit a
; negative (high bit set) number.

fxcopy:     ldy #$00
fxcopyloop: lda $4000, y
            sta (ZPtrA), y
            bmi :+
            iny
            bne fxcopyloop
:           rts

BarCount:   .byte   0
HarmAdvA:   .byte   0
HarmAdvB:   .byte   0

Note0C  = $10
Note1C  = $20
Note1D  = $24
Note1E  = $29   ; 28.5
Note1F  = $2B   ; 2A.667
Note1G  = $30
Note2A  = $36
Note2B  = $3D   ; 3C.75
Note2C  = $40
Note2D  = $48
Note2E  = $51
Note2F  = $55   ; 55.333
Note2G  = $60
Note3A  = $6C
Note3B  = $7A   ; 79.5
Note3C  = $80

; create a superposition of two sin waves
; store it at ZPtrA, assumes it is at a page boundary
; one harmonic is in X, one is in Y
; harmonic numbers make sense only up to $40 really.
; the numbers refer to how far along the sin table each note skips
; so the frequency of 2 is twice 1, 10 is twice 8, 40 is twice 20.
; if we start with 20, we should get a major scale with
; 20 - 24 - 28.5 - 2A.667 - 30 - 36 - 3C.75 - 40
; could try this but I think it wont work.
; 40 - 48 - 51 - 55.333 - 60 - 6C - 79.5 - 80
; 40 - 48 - 51 - 55 - 60 - 6C - 7A - 80 approx
twosin:     sta BarCount
            stx HarmAdvA
            sty HarmAdvB
            ldx #$00
            ldy #$00
twosinb:    lda HarmAdvA
            beq :+                  ; note a is silent
            lda SinTable, y
:           sta ZPxScratch
            lda HarmAdvB
            beq :+                  ; note b is silent
            lda SinTable, x
:           clc
            adc ZPxScratch          ; should be max $3E
            sty ZPxScratch
            ldy #$00
            sta (ZPtrA), y
            ldy ZPxScratch
            txa
            clc
            adc HarmAdvA
            tax
            tya
            clc
            adc HarmAdvB
            tay
            inc ZPtrA
            bne twosinb
            inc ZPtrA + 1
            dec BarCount
            bpl twosinb
            rts

SeqIndex:   .byte   0

; enter seqsin with X and Y holding L and H pointer to sequence
; and A holding page for sample destination
seqsin:     stx seqloop + 1
            stx seqdur + 1
            stx seqb + 1
            sty seqloop + 2
            sty seqdur + 2
            sty seqb + 2
            sta ZPtrA + 1
            ldx #$00
            stx ZPtrA
            stx SeqIndex
seqloop:    ldy MusSeqA, x
            inx
seqdur:     lda MusSeqA, x
            beq seqdone
            pha
            inx
seqb:       lda MusSeqA, x
            tax
            pla
            jsr twosin
            lda SeqIndex
            clc
            adc #$03
            sta SeqIndex
            tax
            jmp seqloop
seqdone:    rts

SFXHeroGot  = $1F       ; 1F - FXHeroGot - hero got a disk
SFXHrdrGot  = $1E       ; 1E - FXHrdrGot - hoarder got a disk
SFXDoh      = $1D       ; 1D - FXDoh - error sound (drop something you do not have)
SFXDrop     = $1C       ; 1C - FXDrop - drop disk

soundinit:  lda #$81    ; bank 1
            sta ZPtrA + XByte
            lda #$00
            sta ZPtrA
            ; 1F - FXHeroGot - hero got a disk
            lda #SFXHeroGot
            sta ZPtrA + 1
            lda #<FXHeroGot
            sta fxcopyloop + 1
            lda #>FXHeroGot
            sta fxcopyloop + 2
            jsr fxcopy
            ; 1E - FXHrdrGot - hoarder got a disk
            lda #SFXHrdrGot
            sta ZPtrA + 1
            lda #<FXHrdrGot
            sta fxcopyloop + 1
            lda #>FXHrdrGot
            sta fxcopyloop + 2
            jsr fxcopy
            ; 1D - FXDoh - error sound (drop something you do not have)
            lda #SFXDoh
            sta ZPtrA + 1
            lda #<FXDoh
            sta fxcopyloop + 1
            lda #>FXDoh
            sta fxcopyloop + 2
            jsr fxcopy
            ; 1C - FXDrop - drop disk
            lda #SFXDrop
            sta ZPtrA + 1
            lda #<FXDrop
            sta fxcopyloop + 1
            lda #>FXDrop
            sta fxcopyloop + 2
            jsr fxcopy
            ; background segments
            ; transform the sin table from 8 bits to at most 5, suitable for adding without clipping
            ; but I continued to reduce the amplitude to make the music quieter
            ldy #$00
:           lda SinTable, y
            lsr                     ; 7 bits (max $7F)
            lsr                     ; 6 bits (max $3F)
            lsr                     ; 5 bits (max $1F) suitable for adding
            lsr                     ; 4 bits (max $0F) make it quieter
            lsr                     ; 3 bits (max $07) like a lot quieter
            sta SinTable, y
            iny
            bne :-
            ; segment 1 (2000-3FFF)
            lda #$20
            ldx #<MusSeqA
            ldy #>MusSeqA
            jsr seqsin
            ; segment 2 (5000-?)
            lda #$50
            ldx #<MusSeqB
            ldy #>MusSeqB
            jsr seqsin
            ; segment 3 (6000-7FFF)
            rts

; sequence tables for generating music samples
; these are set up in units of $100-byte pages, so a duration of 2 burns $200 bytes.
; TODO allow for faster subdivisions.  $1000 bytes goes by in around 4 seconds.

MusSeqA:    .byte   Note1C, 2, Note2C
            .byte   Note1C, 2, Note2E
            .byte   Note1C, 2, Note2G
            .byte   Note1C, 4, Note2E
            .byte   Note1C, 2, Note2C
            .byte   Note1C, 2, Note2G
            .byte   Note1C, 4, Note2E
            .byte   Note1C, 2, Note2C
            .byte   Note1E, 2, Note2G
            .byte   Note1E, 2, Note2E
            .byte   Note1G, 2, Note2G
            .byte   Note1G, 2, Note2E
            .byte   Note1G, 1, Note2C   ; I don't really understand why these
            .byte   Note1G, 1, Note2E   ; come out sounding like triplets.
            .byte   Note1G, 1, Note2G
            .byte   0, 0, 0

MusSeqB:    .byte   Note2C, 8, Note2C + 1
            .byte   Note1C, 8, Note1C + 1
            .byte   Note0C, 4, Note1E
            .byte   Note2C, 2, Note2C + 1
            .byte   Note2C, 2, Note2C + 2
            .byte   Note2C, 1, Note2C + 3
            .byte   Note2C, 1, Note2C + 4
            .byte   Note1C, 12, Note1C + 1
            .byte   0, 0, 0

; this is just a constructed sample for the moment
; sample is over when reaches a number with hi bit set
; samples here cannot be very long, a single byte indexes where we are.

FXHeroGot:
            .byte   $0, $3F,  $0, $3F, $0, $3F, $0,  $3F, $0
            .byte   $3F, $0,  $3F, $0, $3F, $0,  $3F, $0, $3F
            .byte   $80

FXHrdrGot:
            .byte   $10, $18, $20, $20, $18, $10, $10, $10 
            .byte   $10, $18, $20, $20, $18, $10, $10, $10 
            .byte   $10, $18, $20, $20, $18, $10, $10, $10 
            .byte   $10, $18, $20, $20, $18, $10, $10, $10 
            .byte   $10, $28, $30, $40, $38, $20, $20, $10 
            .byte   $10, $28, $30, $40, $38, $20, $20, $10 
            .byte   $10, $28, $30, $40, $38, $20, $20, $10 
            .byte   $10, $28, $30, $40, $38, $20, $20, $10 
            .byte   $10, $18, $20, $20, $18, $10, $10, $10 
            .byte   $10, $18, $20, $20, $18, $10, $10, $10 
            .byte   $10, $18, $20, $20, $18, $10, $10, $10 
            .byte   $10, $18, $20, $20, $18, $10, $10, $10
            .byte   $10, $28, $30, $40, $38, $20, $20, $10 
            .byte   $10, $28, $30, $40, $38, $20, $20, $10 
            .byte   $10, $28, $30, $40, $38, $20, $20, $10 
            .byte   $10, $28, $30, $40, $38, $20, $20, $10 
            .byte   $80
            ; $80 ($B6)

FXDrop:
            .byte   $0, $10, $3F, $10, $0, $10, $3F, $10, $0
            .byte   $10, $3F, $10, $0, $10, $3F, $10, $0, $10
            .byte   $0, $10, $3F, $5, $0, $3F, $20, $10, $3F
            .byte   $10, $3F, $20, $0, $30, $3F, $20, $3F, $10
            .byte   $80

FXDoh:
            .byte   $00, $00, $00, $00, $3F, $00, $00, $00, $3F, $00, $00, $00, $2F
            .byte   $00, $00, $00, $00, $3F, $00, $00, $00, $3F, $00, $00, $00, $2F
            .byte   $80

SinTable:
            .byte   $80, $83, $86, $89, $8C, $90, $93, $96
            .byte   $99, $9C, $9F, $A2, $A5, $A8, $AB, $AE
            .byte   $B1, $B3, $B6, $B9, $BC, $BF, $C1, $C4
            .byte   $C7, $C9, $CC, $CE, $D1, $D3, $D5, $D8
            .byte   $DA, $DC, $DE, $E0, $E2, $E4, $E6, $E8
            .byte   $EA, $EB, $ED, $EF, $F0, $F1, $F3, $F4
            .byte   $F5, $F6, $F8, $F9, $FA, $FA, $FB, $FC
            .byte   $FD, $FD, $FE, $FE, $FE, $FF, $FF, $FF
            .byte   $FF, $FF, $FF, $FF, $FE, $FE, $FE, $FD
            .byte   $FD, $FC, $FB, $FA, $FA, $F9, $F8, $F6
            .byte   $F5, $F4, $F3, $F1, $F0, $EF, $ED, $EB
            .byte   $EA, $E8, $E6, $E4, $E2, $E0, $DE, $DC
            .byte   $DA, $D8, $D5, $D3, $D1, $CE, $CC, $C9
            .byte   $C7, $C4, $C1, $BF, $BC, $B9, $B6, $B3
            .byte   $B1, $AE, $AB, $A8, $A5, $A2, $9F, $9C
            .byte   $99, $96, $93, $90, $8C, $89, $86, $83
            .byte   $80, $7D, $7A, $77, $74, $70, $6D, $6A
            .byte   $67, $64, $61, $5E, $5B, $58, $55, $52
            .byte   $4F, $4D, $4A, $47, $44, $41, $3F, $3C
            .byte   $39, $37, $34, $32, $2F, $2D, $2B, $28
            .byte   $26, $24, $22, $20, $1E, $1C, $1A, $18
            .byte   $16, $15, $13, $11, $10, $0F, $0D, $0C
            .byte   $0B, $0A, $08, $07, $06, $06, $05, $04
            .byte   $03, $03, $02, $02, $02, $01, $01, $01
            .byte   $01, $01, $01, $01, $02, $02, $02, $03
            .byte   $03, $04, $05, $06, $06, $07, $08, $0A
            .byte   $0B, $0C, $0D, $0F, $10, $11, $13, $15
            .byte   $16, $18, $1A, $1C, $1E, $20, $22, $24
            .byte   $26, $28, $2B, $2D, $2F, $32, $34, $37
            .byte   $39, $3C, $3F, $41, $44, $47, $4A, $4D
            .byte   $4F, $52, $55, $58, $5B, $5E, $61, $64
            .byte   $67, $6A, $6D, $70, $74, $77, $7A, $7D

