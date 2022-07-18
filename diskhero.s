; DISKHERO
; Paul Hagstrom, 2022

            .segment "CODE"
            .setcpu "6502"

            .include "diskhero.inc"

; We have from A000 to B800 before SOS arrives (6144 bytes), anything up there will
; be safe from bank switching.  The program and data is too large for that, so I start
; lower, but trying to put everything that needs to be bank-switch safe up in the higher
; end.  Anything that pushes data to the hires graphics pages pretty much needs to be
; bank-safe, since bank 0 needs to be active in order to use stack/ZP to push pixels.

; Screen layout:
; mode 6 (bw hires)  lines 00-0F (10)        b/w "splash" screen
; mode 1 (text)      lines 10-1F (10) 02-03  score/status
; mode 7 (a3 hires)  lines 20-3F (20)        hires map upper field  map: 00-1F
; mode 1 (text)      lines 40-8F (50) 08-11  text play field        map: 20-27
; mode 7 (a3 hires)  lines 90-AF (20)        hires map lower field  map: 28-47
; mode 5 (a3 medres) lines B0-BF (18) 15-17  medium res something

; Notes to self about how much space I am allowed by starting below A000.
; start leaves  start   leaves  start   leaves
; 9F00  6400    9E00    6656    9D00    6912
; 9C00  7167    9B00    7423    9A00    7679
; 9900  7935    9800    8191    9700    8447

            .org     $9700 - 14
            
; SOS interpreter header
            .byte    "SOS NTRP"
            .word    0000
            .word    CodeStart
            .word    (CodeEnd-CodeStart)

CodeStart:  jmp init

            .include "buildmap.s"           ; should be safe in banked memory
            .include "gamesound.s"          ; should be safe in banked memory
            .include "status-text40.s"      ; should be safe in banked memory
            .include "gamemove.s"           ; probably ok in banked memory
            .include "play-text40.s"        ; should be safe in banked memory
            .include "reg-superhires.s"     ; should be safe in banked memory
            .include "reg-medres.s"         ; should be safe in banked memory
            .include "gamefont.s"           ; should be safe in banked memory
            .include "map-hires3-data.s"    ; cannot be in banked memory
            .include "map-hires3.s"         ; cannot be in banked memory
            .include "interrupts.s"         ; cannot be in banked memory
            .include "lookups.s"            ; cannot be in banked memory

IRQSave:    .byte   0, 0 , 0        ; saved state
ZPSave:     .byte   0
BankSave:   .byte   0

ExitFlag:   .byte   0               ; keyboard int makes this nonzero to trigger exit
KeyCaught:  .byte   0               ; keyboard int pushes a caught key in here

VoidL:      .byte   0               ; keeping track of "void" around the map while drawing
VoidR:      .byte   0
VoidU:      .byte   0
VoidD:      .byte   0

GameLevel:  .byte   0
GameScore:  .byte   0, 0, 0
DisksGot:   .byte   0, 0, 0, 0
DisksLeft:  .byte   0, 0, 0, 0

NumHoards:  .byte   0
NumDisks:   .byte   0

MapColors:  .byte   $88, $66, $33, $44      ; color bytes for indexed shapes (walls)
DiskColors: .byte   $EE, $DD, $CC, $BB      ; color bytes for indexed disks specifically
HeroX:      .byte   0                       ; X-coordinate of player on the map.
HeroY:      .byte   0                       ; Y-coordinate of player on the map.
VelocityX:  .byte   0                       ; X-velocity of player (neg, 0, pos)
VelocityY:  .byte   0                       ; Y-velocity of player (neg, 0, pos)
MoveDelay   = 1                             ; VBL tick delay between moves

CurScrLine: .byte   0
CurMapLine: .byte   0
MapPtrL:    .byte   0                       ; Holds address of left edge of a map line (low)
MapPtrH:    .byte   0                       ; Holds address of left edge of a map line (high)

Seed:       .byte   0                       ; current place in the "random" number table

eventloop:  lda ExitFlag
            bne alldone
            lda KeyCaught
            beq :+
            jsr handlekey
:            
            jsr drawstatus
            lda VBLTick
            bpl posttick
            jsr domove
            jsr updsplash
            lda #MoveDelay
            sta VBLTick
posttick:   jmp eventloop

alldone:    lda #$7F            ;disable all interrupts
            sta RD_INTENAB
            sta RD_INTFLAG
            lda #$7F
            sta RE_INTENAB
            sta RE_INTFLAG

            brk                  ; SOS TERMINATE
            .byte   TERMINATE
            .word   *-2

; process keypress

handlekey:  
            cmp #$D5            ; U (up-left)
            bne :+
            lda #$80
            sta VelocityX
            sta VelocityY
            jmp keydone
:           cmp #$C9            ; I (up, scroll map down)
            bne :+
            lda #$00
            sta VelocityX
            lda #$80
            sta VelocityY
            jmp keydone
:           cmp #$CF            ; O (up-right)
            bne :+
            lda #$01
            sta VelocityX
            lda #$80
            sta VelocityY
            jmp keydone
:           cmp #$CA            ; J (left)
            bne :+
            lda #$80
            sta VelocityX
            lda #$00
            sta VelocityY
            jmp keydone
:           cmp #$CB            ; K (stop)
            bne :+
            lda #$00
            sta VelocityX
            sta VelocityY
            jmp keydone
:           cmp #$CC            ; L (right)
            bne :+
            lda #$01
            sta VelocityX
            lda #$00
            sta VelocityY
            jmp keydone
:           cmp #$CD            ; M (down-left)
            bne :+
            lda #$80
            sta VelocityX
            lda #$01
            sta VelocityY
            jmp keydone
:           cmp #$AC            ; , (down, scroll map up)
            bne :+
            lda #$00
            sta VelocityX
            lda #$01
            sta VelocityY
            jmp keydone
:           cmp #$AE            ; . (down-right)
            bne :+
            lda #$01
            sta VelocityX
            sta VelocityY
            jmp keydone
:           cmp #$D3            ; S (soundtrack)
            bne :+
            lda ZPlaySound
            eor #$01
            sta ZPlaySound
            jmp keydone
:           cmp #$D1            ; Q (quiet)
            bne :+
            lda ZPlaySFX
            eor #$01
            sta ZPlaySFX
            jmp keydone
:           cmp #$B1            ; 1 (drop disk type 1)
            bne :+
            ldx #$00
            jsr dropdisk
            jmp keydone
:           cmp #$B2            ; 2 (drop disk type 2)
            bne :+
            ldx #$01
            jsr dropdisk
            jmp keydone
:           cmp #$B3            ; 3 (drop disk type 3)
            bne :+
            ldx #$02
            jsr dropdisk
            jmp keydone
:           cmp #$B4            ; 4 (drop disk type 4)
            bne :+
            ldx #$03
            jsr dropdisk
            jmp keydone
:           cmp #$C5            ; E (exit)
            bne keydone            
            inc ExitFlag        ; tell event loop we are exiting
keydone:    lda #$00
            sta KeyCaught
            rts

; add to score, add the number in A to the score

addscore:   ldx #$02
            sed
            clc
            adc GameScore, x
            sta GameScore, x
            dex
            lda GameScore, x
            adc #$00
            sta GameScore, x
            dex
            lda GameScore, x
            adc #$00
            sta GameScore, x
            cld
            rts

; compute map pointer, based on A.  Map data is in bank 2, $2000-5FFF.
; If current map pointer is something like 00000101 (5), shift bits to translate to:
; MapPtrL: 01000000 (40) MapPtrH: 00010001 (11) ($1140 and $40 bytes there)

setmapptr:  pha
            lda #$00
            sta MapPtrL
            pla
            lsr                 ; shift lower two bits of map line
            ror MapPtrL         ; into higher bits of MPL
            lsr                 ; (multiplying by $40, the length of a map line)
            ror MapPtrL
            clc
            adc #$20            ; map data starts at $2000.
            sta MapPtrH
            rts

; grab a random number seed from the fastest part of the realtime clock.
; I don't think this actually works, but something like this would be a good idea.
seedRandom: lda #$00
            sta R_ZP        ; request smallest RTC byte
            lda IO_CLOCK    ; close enough to random for now
            sta Seed
            lda #$1A
            sta R_ZP
            rts

; initialize graphics regions and draw static background
initscreen: bit D_PAGEONE
            jsr initstatus      ; text lines 02-03: score/status
            jsr initplay        ; text lines 08-11: playfield (draw frame)
            jsr drawplay        ; draw actual playfield
            jsr initshgr        ; mode 6 super hires line 00-0F (doesn't swap in bank 0)
            jsr initmedres      ; med res lines B0-BF (doesn't swap in bank 0)
            jsr initmap         ; mode 7 a3 hires map regions
            rts

; initialize initial environment and game variables

init:       sei                 ; no interrupts while we are setting up
            ;     0------- 2MHz clock (1=1MHz)
            ;     -1------ C000.CFFF I/O (0=RAM)
            ;     --1----- video enabled (0=disabled)
            ;     ---1---- Reset key enabled (0=disabled)
            ;     ----0--- C000.CFFF read/write (1=read only)
            ;     -----1-- True stack ($100) (0=alt stack)
            ;     ------1- ROM#1 (0=ROM#2)
            ;     -------1 F000.FFFF RAM (1=ROM)
            lda #%01110111      ; 2MHz, video, I/O, reset, r/w, ram, ROM#1, true stack
            sta R_ENVIRON
            lda #$20            ; start playfield kind of in the middle
            sta HeroX
            lda #$C3            ; Start down near the bottom (note: number mod 8 should be 3)
            sta HeroY
            jsr soundinit       ; move and generate sounds (in bank 1)
            jsr herofont        ; load game font into character RAM and fill ZFontDats, ZFontCol
            jsr buildmap        ; set up map data (in bank 2) (includes dropping in the hero)
            jsr setupenv        ; arm interrupts
            lda #$00            ; start at nudge 0
            sta NudgePos
            sta GameLevel
            sta GameScore
            sta GameScore + 1
            sta GameScore + 2
            sta VelocityX
            sta VelocityY
            lda #MoveDelay       ; set how many VBLs go by before movement advances
            sta VBLTick
            bit IO_KEYCLEAR
            jsr initscreen      ; draw the initial screen
            cli                 ; all set up now, interrupt away
            jmp eventloop

CodeEnd     = *