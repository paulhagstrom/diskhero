; DISKHERO
; Paul Hagstrom, 2022

            .segment "CODE"
            .setcpu "6502"

            .include "diskhero.inc"

; This program is absolutely not playing by the rules.  Partly because I don't really
; know the rules.  It takes over the machine, so it has almost no interaction with SOS.
; it invades SOS's reserved space as well.  The area from $A000-B800 is below where
; SOS actually is, but Apple doesn't want you using it in case a future version of SOS
; needs it.  But I think I will risk it.  It's an area that is safe from bank switching,
; so I try to wedge as much of the code up there as I can.  This also does not return to
; SOS in any sensible way at present, though it does make an attempt to call a SOS
; "terminate" command at the end.
;
; Screen layout:
; mode 6 (bw hires)  lines 00-0F (10)        b/w "splash" screen
; mode 1 (text)      lines 10-1F (10) 02-03  score/status
; mode 7 (a3 hires)  lines 20-3F (20)        hires map upper field  map: 00-1F
; mode 1 (text)      lines 40-8F (50) 08-11  text play field        map: 20-27
; mode 7 (a3 hires)  lines 90-AF (20)        hires map lower field  map: 28-47
; mode 5 (a3 medres) lines B0-BF (18) 15-17  medium res something

; Notes to self about how much space I am allowed by starting below A000.
; But after I compile I can check the size of the resulting binary and see if it still
; fits, or whether I need to bump it down yet another page.
; start leaves  start   leaves  start   leaves
; 9F00  6400    9E00    6656    9D00    6912
; 9C00  7167    9B00    7423    9A00    7679
; 9900  7935    9800    8191    9700    8447

            .org     $9600 - 14
            
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

IRQSave:    .byte   0, 0 , 0                ; IRQ vector before we install ours

ExitFlag:   .byte   0                       ; keyboard int makes this nonzero to trigger exit
KeyCaught:  .byte   0                       ; keyboard int pushes a caught key in here

VoidL:      .byte   0                       ; keeping track of "void" around the map while drawing
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

; schedule of background music samples
MusicSeq:   .byte   $20, $50, $50, $20, $00
NowPlaying: .byte   0                       ; current position on the MusicSeq list

eventloop:  lda ExitFlag                    ; if ExitFlag becomes nonzero (within keyboard processing)
            bne alldone                     ; exit
            lda KeyCaught                   ; check if we have recently caught a key (keyboard interrupt)
            beq :+
            jsr handlekey                   ; if there was a key, handle it
:            
            lda VBLTick                     ; wait for game clock to tick
            bpl posttick                    ; based on number of VBLs set in MoveDelay
            jsr domove                      ; game clock has ticked, move everyone around
            jsr updsplash                   ; update the splash effect at the top
            jsr drawstatus                  ; redraw score (TODO: do only when there is an update)
            lda BackNext                    ; is there a background sample already queued up?
            bne elmusicok                   ; yep we're all good
            ldx NowPlaying                  ; find the next segment
            inx
            lda MusicSeq, x
            bne elsetnext                   ; got the next segment
            ldx #$00                        ; hit the end, go back to the first segment
            lda MusicSeq
elsetnext:  stx NowPlaying
            sta BackNext                    ; queue up the next one
elmusicok:  lda #MoveDelay                  ; reset the game clock
            sta VBLTick
posttick:   jmp eventloop

alldone:    lda #$7F                        ;disable all interrupts
            sta RD_INTENAB
            sta RD_INTFLAG
            lda #$7F
            sta RE_INTENAB
            sta RE_INTFLAG

            brk                             ; SOS TERMINATE
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
:           cmp #$D3            ; S (sound)
            bne :+
            lda PlaySound
            eor #$01
            sta PlaySound
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

; subtract from score, remove the number in A from the score

subscore:   ldx #$02
            sta subop + 1
            sed
            lda GameScore, x
            sec
subop:      sbc #$00
            sta GameScore, x
            dex
            lda GameScore, x
            sbc #$00
            sta GameScore, x
            dex
            lda GameScore, x
            sbc #$00
            sta GameScore, x
            cld
            rts

; compute map pointer, based on A upon entry.  Map data is in bank 2, $2000-5FFF.
; each line is $40 bytes in storage (only 3F bytes used), so take A and multiply by
; $40.  This is equivalent to rotating A right twice into a zero and swapping the bytes.
; If current map pointer is something like 00000101 (5), shift bits to translate to:
; MapPtrL: 01000000 (40) MapPtrH: 00100001 (11) ($2140 and $40 bytes there)

setmapptr:  pha
            lda #$00
            sta MapPtrL
            pla
            lsr                 ; shift lower two bits of map line
            ror MapPtrL         ; into higher bits of MPL
            lsr                 ; (multiplying by $40, the length of a map line)
            ror MapPtrL
            clc
            adc #$20            ; map data starts at $2000, add this to H.
            sta MapPtrH
            rts

; grab a random number seed from the fastest part of the realtime clock.
; I don't think this actually works, but something like this would be a good idea.
seedRandom: lda #$00
            sta R_ZP            ; request smallest RTC byte
            lda IO_CLOCK        ; close enough to random for now
            sta Seed            ; Seed is just used as the index into the "random" number table
            lda #$1A            ; return the ZP to $1A00
            sta R_ZP
            rts

; initialize graphics regions and draw static background
initscreen: bit D_PAGEONE       ; be on page 1.  Nothing presently uses page 2 for anything.
            jsr initstatus      ; text lines 02-03: score/status
            jsr initplay        ; text lines 08-11: playfield (draw frame)
            jsr drawplay        ; draw actual playfield
            jsr initshgr        ; mode 6 super hires line 00-0F (doesn't swap in bank 0)
            jsr initmedres      ; med res lines B0-BF (doesn't swap in bank 0)
            jsr initmap         ; mode 7 a3 hires map regions - paint whole visible map
            rts

; initialize initial environment and game variables

init:       sei                 ; no interrupts while we are setting up
            ;     0------- 2MHz clock           (1=1MHz)
            ;     -1------ C000.CFFF I/O        (0=RAM)
            ;     --1----- video enabled        (0=disabled)
            ;     ---1---- Reset key enabled    (0=disabled)
            ;     ----0--- C000.CFFF read/write (1=read only)
            ;     -----1-- True stack ($100)    (0=alt stack)
            ;     ------1- ROM#1                (0=ROM#2)
            ;     -------1 F000.FFFF RAM        (1=ROM)
            lda #%01110111      ; 2MHz, video, I/O, reset, r/w, ram, ROM#1, true stack
            sta R_ENVIRON
            lda #$20            ; start playfield kind of in the middle
            sta HeroX           ; this is the X coordinate of the hero on the map (0-3E)
            lda #$C3            ; Start down near the bottom (note: number mod 8 should be 3)
            sta HeroY           ; this is the Y coordinate of the hero on the map (0-FF)
            jsr soundinit       ; generate sounds and move them (in bank 1)
            jsr herofont        ; load game font into character RAM and fill ZFontDats, ZFontCol
            jsr buildmap        ; set up map data (in bank 2) (includes dropping the hero in)
            jsr setupenv        ; arm interrupts
            ldx TwelveBran      ; start at smooth scroll offset 0 (first one in TwelveBran)
            sta NudgeVal        ; the smooth scroll offset is affectionately called the "nudge"
            sta GameLevel
            sta GameScore       ; score is stored in decimal format. 000000 to 999999.
            sta GameScore + 1
            sta GameScore + 2
            sta VelocityX       ; hero X velocity, can be negative, zero, or positive
            sta VelocityY       ; hero Y velocity, can be negative, zero, or positive
            lda #MoveDelay      ; game clock - setting number of VBLs per movement advance
            sta VBLTick
            bit IO_KEYCLEAR     ; clear keyboard so we notice any new keypress
            lda MusicSeq        ; start with first sound segment in the sequence
            sta ZSoundPtr + 1   ; background music segment we are currently playing, high byte
            lda #$00            ; 
            sta BackNext        ; let event loop queue the next segment
            sta NowPlaying      ; currently at first step of MusicSeq
            sta ZSoundPtr       ; background music segment we are currently playing, low byte
            jsr initscreen      ; draw the initial screen
            cli                 ; all set up now, commence interrupting
            jmp eventloop       ; wait around until it is time to quit

CodeEnd     = *