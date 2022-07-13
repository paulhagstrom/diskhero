; DISKHERO
; Paul Hagstrom, 2022

            .segment "CODE"
            .setcpu "6502"

            .include "diskhero.inc"

; we have from A000 to B800 before SOS arrives (6144 bytes)
; I can fudge this a little if needed, by starting with JMP and putting data early,
; since the main concern is trying to run code in a bank switched area.

            .org     $9F00 - 14
            
; SOS interpreter header
            .byte    "SOS NTRP"
            .word    0000
            .word    CodeStart
            .word    (CodeEnd-CodeStart)

CodeStart:  jmp init

            .include "gamefont.s"
            .include "lookups.s"
            .include "interrupts.s"
            .include "gamemove.s"
            .include "buildmap.s"
            .include "status-text40.s"
            .include "reg-superhires.s"
            .include "play-text40.s"
            .include "map-hires3.s"
            .include "reg-medres.s"
            .include "status-text80.s"
            .include "gamesound.s"

IRQSave:    .byte   0, 0 , 0        ; saved state
ZPSave:     .byte   0
BankSave:   .byte   0

ExitFlag:   .byte   0               ; keyboard int makes this nonzero to trigger exit
KeyCaught:  .byte   0               ; keyboard int pushes a caught key in here
RedrawMap:  .byte   0               ; keyboard int makes this nonzero to trigger redraw
RedrawPlay: .byte   0               ; keyboard int makes this nonzero to trigger redraw
CurrMap:    .byte   0
VoidL:      .byte   0
VoidR:      .byte   0
VoidU:      .byte   0
VoidD:      .byte   0

GameLevel:  .byte   0
GameScore:  .byte   0, 0, 0
DisksGot:   .byte   0, 0, 0, 0
DisksLeft:  .byte   0, 0, 0, 0
NumHoards:  .byte   0

FieldH:     .byte   $04, $05, $05, $06, $06, $07
FieldL:     .byte   $A8, $25, $A8, $28, $A8, $28
FieldHC:    .byte   $08, $09, $09, $0A, $0A, $0B
MapColors:  .byte   $88, $66, $33, $44
DiskColors: .byte   $EE, $DD, $CC, $BB
; Screen layout:
; mode 1 (text)      lines 00-0F (10) 00-01  score/status
; mode 6 (bw hires)  lines 10-1F (10)        b/w map display
; mode 7 (a3 hires)  lines 20-3F (20)        hires map upper field  map: 00-1F
; mode 1 (text)      lines 40-87 (48) 08-10  text play field        map: 20-26
; mode 7 (a3 hires)  lines 88-A7 (20)        hires map lower field  map: 27-46
; mode 3 (80 text)   linwa A8-AF (08) 15     b/w 80 column text
; mode 5 (a3 medres) lines B0-BF (18) 15-17  medium res something
;
; Define the screen region; mode is a display mode, length is number of HBLs.
; nudge is 0 if no nudge, else pos or neg depending on which nudge count to use
HeroX:      .byte   0
HeroY:      .byte   0
HeroDir:    .byte   0
VelocityX:  .byte   0
VelocityY:  .byte   0
MoveDelay   = 1            ; VBL tick delay between moves

; I played with ScRegLen by trial and error a little.
; Not sure why I needed to go two down for region zero.
; Probably because the VBL code (or HBL code) takes long enough that we miss
; some HBLs here and there.
; Also worth noting that it is not fully consistent between MAME and real hardware.
; May want to put some black gaps between switches to fudge that a little, so that
; I don't have to wait for MAME to catch up (and can continue to develop in MAME)
; Since I suspect it is somewhat dependent on workload (some HBLs are missed?) may want
; to wait until it is closer to operational to tweak that.

CurScrLine: .byte   0
CurMapLine: .byte   0
MapPtrL:    .byte   0
MapPtrH:    .byte   0
StashZP:    .byte   0

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
            lda #$1A            ; ZP to $1A00 (standard for interpreters, should already be here)
            sta R_ZP
            jsr herofont        ; load game font into character RAM
            ldx #$27            ; pull the FontDots and FontCol info into ZP for faster access
:           lda FontDots, x
            sta ZFontDots, x
            lda FontCol, x
            sta ZFontCol, x
            dex
            bpl :-
            lda #$10            ; start playfield kind of in the middle
            sta HeroX
            lda #$C3            ; Start down near the bottom but at a nudge 0 spot
            sta HeroY
            ; point memory at location tracking in bank 2 (with map)
            ; DiskX = 300, DiskY = 340, DiskType = 380
            ; HoardX = 400, HoardY = 440, HoardXV = 480, HoardYV = 4C0,
            ; HoardSp = 500, HoardXX = 540, HoardYY = 580, HoardTick = 5C0,
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
            jsr buildmap        ; set up map
            jsr setupenv        ; arm interrupts
            lda #$00
            sta ScrRegion
            lda #$00            ; start at nudge 0
            sta NudgePos
            sta NudgeNeg
            sta GameLevel
            sta GameScore
            sta GameScore + 1
            sta GameScore + 2
            sta VelocityX
            sta VelocityY
            sta HeroDir
            lda #MoveDelay       ; set how many VBLs go by before movement advances
            sta VBLTick
            lda #$01
            sta RedrawMap       ; start by assuming we need to redraw whole thing
            sta RedrawPlay      ; start by assuming we need to redraw whole thing
            bit IO_KEYCLEAR
            jsr initscreen      ; draw the initial screen
            cli                 ; all set up now, interrupt away
eventloop:  lda ExitFlag
            bne alldone
            lda KeyCaught
            beq :+
            jsr handlekey
:            
            inc $427            ; DEBUG - constantly churn screen memory to detect hang
            jsr drawstatus
            lda VBLTick
            bpl posttick
            jsr domove
            dec NudgeNeg
            bpl :+
            lda #$07
            sta NudgeNeg
:           lda #MoveDelay
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

Seed:        .byte    0

seedRandom:
            ; grab a random number seed from the fastest part of the realtime clock.
            lda #$00
            sta R_ZP        ; request smallest RTC byte
            lda IO_CLOCK    ; close enough to random for now
            sta Seed
            lda #$1A
            sta R_ZP
            rts

; initialize graphics regions and draw static background
initscreen:
            lda R_ZP
            sta ZPSave          ; save current ZP
            lda R_BANK
            sta BankSave        ; save current bank
            lda #$00            ; swap in bank zero,
            sta R_BANK          ; where (hires) graphics memory lives
            lda #$01            ; Apple III color text
            jsr setdisplay
            bit D_PAGEONE
            bit D_SCROLLOFF
            lda #$00
            jsr setnudge
            jsr initstatus      ; text lines 00-01: score/status
            jsr initmsg         ; text line 21: message
            jsr initshgr        ; mode 6 super hires line 10-1F
            jsr initmedres      ; med res lines B0-BF
            jsr initmap         ; mode 7 a3 hires map regions
            jsr initplay        ; text lines 08-11: playfield (draw frame)
            jsr drawplay        ; draw actual playfield
            lda #$00            ; reset the update flags
            sta RedrawMap
            sta RedrawPlay
            lda ZPSave          ; restore ZP and bank
            sta R_ZP
            lda BankSave
            sta R_BANK
            rts

; Set smooth scroll offset to the number (0-7) in A
; implicitly performs a mod 8 (higher bits don't matter)

setnudge:   ror
            bcs snxxy
            bit SS_XXN
            ror
            bcs snxyx
snxnx:      bit SS_XNX
            ror
            bcs snyxx
snnxx:      bit SS_NXX
            rts
snxxy:      bit SS_XXY
            ror
            bcc snxnx     
snxyx:      bit SS_XYX
            ror
            bcc snnxx
snyxx:      bit SS_YXX
            rts

; set display to number in A
; 0 = 40 char Apple II b/w                  gr      nomix   lores
; 1 = 40 char Apple III color               text    nomix   lores
; 2 = 80 char b/w                           gr      mix     lores
; 3 = 80 char b/w                           text    mix     lores
; 4 = Apple II hires (280x192 b/w)          gr      nomix   hires
; 5 = Fg/bg hires (280x192, 16 colors)      text    nomix   hires
; 6 = super hires (560x192, b/w)            gr      mix     hires
; 7 = 140x192 A Hires (140x192, color)      text    mix     hires

setdisplay: ror
            bcs sdtext
            bit D_GRAPHICS
            ror
            bcs sdmix
sdnomix:    bit D_NOMIX
            ror
            bcs sdhires
sdlores:    bit D_LORES
            rts
sdtext:     bit D_TEXT
            ror
            bcc sdnomix     
sdmix:      bit D_MIX
            ror
            bcc sdlores
sdhires:    bit D_HIRES
            rts

CodeEnd     = *