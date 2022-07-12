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

initmsg:    ldx #$4F
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
