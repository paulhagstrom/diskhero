; DISKHERO
; sound

; copy or create the sounds in bank 1
; plan is:
; 2000-3FFF: background segment 1
; 4000-5FFF: background segment 2
; 6000-7FFF: background segment 3
; 1F00-1FFF: sound effect 1F
; 1E00-1EFF: sound effect 1E
; ...
; 0300-03FF: sound effect 03

fxcopy:     ldy #$00
fxcopyloop: lda $4000, y
            sta (ZPtrA), y
            bmi :+
            iny
            bne fxcopyloop
:           rts

BarCount:   .byte   0

soundinit:  lda #$81    ; bank 1
            sta ZPtrA + XByte
            lda #$00
            sta ZPtrA
            ; 1F  - FXHeroGot - hero got a disk
            lda #$1F
            sta ZPtrA + 1
            lda #<FXHeroGot
            sta fxcopyloop + 1
            lda #>FXHeroGot
            sta fxcopyloop + 2
            jsr fxcopy
            ; 1E - FXHrdrGot - hoarder got a disk
            lda #$1E
            sta ZPtrA + 1
            lda #<FXHrdrGot
            sta fxcopyloop + 1
            lda #>FXHrdrGot
            sta fxcopyloop + 2
            jsr fxcopy
            ; 1D - FXSthing - something
            lda #$1D
            sta ZPtrA + 1
            lda #<FXSthing
            sta fxcopyloop + 1
            lda #>FXSthing
            sta fxcopyloop + 2
            jsr fxcopy
            ; background segments
            ; segment 1 (2000-3FFF)
            lda #$20
            sta ZPtrA + 1
            lda #$04
            sta BarCount
            ldy #$00
            ldx #$00
backa:      lda SinTable, y
            lsr
            lsr
            lsr
            sta ZPxScratch
            lda SinTable, x
            lsr
            lsr
            lsr
            adc ZPxScratch
            sta (ZPtrA), y
            inx
            inx
            iny
            bne backa
            inc ZPtrA + 1
            dec BarCount
            bpl backa
            lda #$06
            sta BarCount
backab:     lda SinTable, y
            lsr
            lsr
            lsr
            sta ZPxScratch
            lda SinTable, x
            lsr
            lsr
            lsr
            adc ZPxScratch
            sta (ZPtrA), y
            inx
            inx
            inx
            iny
            lda BarCount
            and #$01
            beq :+
            inc ZPtrA + 1
:           dec BarCount
            bpl backab
            lda #$02
            sta BarCount
            lda #$00
backac:     sta (ZPtrA), y
            iny
            bne backac
            lda #$80
            sta (ZPtrA), y       ; done with segment 1
            ; segment 2 (4000-5FFF)
            ; segment 3 (6000-7FFF)
            rts
            
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

FXSthing:
            .byte   $0, $10, $3F, $10, $0, $10, $3F, $10, $0
            .byte   $10, $3F, $10, $0, $10, $3F, $10, $0, $10
            .byte   $0, $10, $3F, $5, $0, $3F, $20, $10, $3F
            .byte   $10, $3F, $20, $0, $30, $3F, $20, $3F, $10
            .byte   $80
            
            
; Above are sound effects
; Could perhaps try creating music
; with a sine table for some frequencies, addition

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

