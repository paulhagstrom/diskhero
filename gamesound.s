; DISKHERO
; sound

; this is just a constructed sample for the moment
; sample is over when reaches a number with hi bit set
; samples here cannot be very long, a single byte indexes where we are.

Samples:    .byte   $80
            .byte   $0, $3F,  $0, $3F, $0, $3F, $0,  $3F, $0
            .byte   $3F, $0,  $3F, $0, $3F, $0,  $3F, $0, $3F
            ; $12
SndHeroGot = *

            .byte   $80
            .byte   $0, $10, $3F, $10, $0, $10, $3F, $10, $0
            .byte   $10, $3F, $10, $0, $10, $3F, $10, $0, $10
            .byte   $0, $10, $3F, $5, $0, $3F, $20, $10, $3F
            .byte   $10, $3F, $20, $0, $30, $3F, $20, $3F, $10
            ; $24 ($36)
SndHrdrGot = *

            .byte   $80
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
            ; $80 ($B6)
            
; Above are sound effects
; Could perhaps try creating music
; with a sine table for some frequencies, addition
