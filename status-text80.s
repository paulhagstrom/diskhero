; DISKHERO
; 80-column text display
; 
; prints game progress messages on the bottom line of the display
; like "Yet another copy of Jeopardy!" or something.

MsgTextA:   .byte "DISKHERO. An Apple /"
            .byte "// game of software "
            .byte "preservation. Or som"
            .byte "ething. As you like."

TEXTMEVEN   = $6D0              ; text line 21 even characters
TEXTMODD    = $AD0              ; text line 21 odd characters
StackSave:  .byte 0
EnvSave:    .byte 0

initmsg:    tsx
            stx StackSave
            lda R_ENVIRON
            sta EnvSave
            and #%11111011      ; zero to get alt stack
            sta R_ENVIRON
            lda #$07            ; ZP $07 puts stack at $06
            sta R_ZP
            ldx #$F7            ; $D0 + $27
            txs
            ldx #$4E
:           lda MsgTextA, x
            pha
            dex
            dex
            bpl :-
            lda #$0B            ; ZP $0B puts stack at 0A
            sta R_ZP
            ldx #$F7            ; $D0 + $27
            txs
            ldx #$4F
 :          lda MsgTextA, x
            pha
            dex
            dex
            bpl :-
            lda #$1A            ; back to interpreter ZP
            sta R_ZP
            lda EnvSave
            sta R_ENVIRON
            ldx StackSave
            txs
            rts
            
; the above gratuitously uses the stack to push the data to the
; screen.  It's probably better in this case to just use absolute addressing.
; smaller code, speed differences that matter little.

initmsgx:    ldx #$4F
            ldy #$27
:           lda MsgTextA, x
            sta TEXTMODD, y
            dex
            lda MsgTextA, x
            sta TEXTMEVEN, y
            dex
            dey
            bpl :-
            rts
