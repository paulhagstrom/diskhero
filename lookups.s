; Graphics lookup tables.

YHiresL:    ; low byte of address of start of each hires line
            .byte $00, $00, $00, $00, $00, $00, $00, $00
            .byte $80, $80, $80, $80, $80, $80, $80, $80
            .byte $00, $00, $00, $00, $00, $00, $00, $00
            .byte $80, $80, $80, $80, $80, $80, $80, $80
            .byte $00, $00, $00, $00, $00, $00, $00, $00
            .byte $80, $80, $80, $80, $80, $80, $80, $80
            .byte $00, $00, $00, $00, $00, $00, $00, $00
            .byte $80, $80, $80, $80, $80, $80, $80, $80
            .byte $28, $28, $28, $28, $28, $28, $28, $28
            .byte $A8, $A8, $A8, $A8, $A8, $A8, $A8, $A8
            .byte $28, $28, $28, $28, $28, $28, $28, $28
            .byte $A8, $A8, $A8, $A8, $A8, $A8, $A8, $A8
            .byte $28, $28, $28, $28, $28, $28, $28, $28
            .byte $A8, $A8, $A8, $A8, $A8, $A8, $A8, $A8
            .byte $28, $28, $28, $28, $28, $28, $28, $28
            .byte $A8, $A8, $A8, $A8, $A8, $A8, $A8, $A8
            .byte $50, $50, $50, $50, $50, $50, $50, $50
            .byte $D0, $D0, $D0, $D0, $D0, $D0, $D0, $D0
            .byte $50, $50, $50, $50, $50, $50, $50, $50
            .byte $D0, $D0, $D0, $D0, $D0, $D0, $D0, $D0
            .byte $50, $50, $50, $50, $50, $50, $50, $50
            .byte $D0, $D0, $D0, $D0, $D0, $D0, $D0, $D0
            .byte $50, $50, $50, $50, $50, $50, $50, $50
            .byte $D0, $D0, $D0, $D0, $D0, $D0, $D0, $D0
                  
YHiresS:    ; low byte of address of END of each hires line (for stack)
            .byte $27, $27, $27, $27, $27, $27, $27, $27
            .byte $A7, $A7, $A7, $A7, $A7, $A7, $A7, $A7
            .byte $27, $27, $27, $27, $27, $27, $27, $27
            .byte $A7, $A7, $A7, $A7, $A7, $A7, $A7, $A7
            .byte $27, $27, $27, $27, $27, $27, $27, $27
            .byte $A7, $A7, $A7, $A7, $A7, $A7, $A7, $A7
            .byte $27, $27, $27, $27, $27, $27, $27, $27
            .byte $A7, $A7, $A7, $A7, $A7, $A7, $A7, $A7
            .byte $4F, $4F, $4F, $4F, $4F, $4F, $4F, $4F
            .byte $CF, $CF, $CF, $CF, $CF, $CF, $CF, $CF
            .byte $4F, $4F, $4F, $4F, $4F, $4F, $4F, $4F
            .byte $CF, $CF, $CF, $CF, $CF, $CF, $CF, $CF
            .byte $4F, $4F, $4F, $4F, $4F, $4F, $4F, $4F
            .byte $CF, $CF, $CF, $CF, $CF, $CF, $CF, $CF
            .byte $4F, $4F, $4F, $4F, $4F, $4F, $4F, $4F
            .byte $CF, $CF, $CF, $CF, $CF, $CF, $CF, $CF
            .byte $77, $77, $77, $77, $77, $77, $77, $77
            .byte $F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7
            .byte $77, $77, $77, $77, $77, $77, $77, $77
            .byte $F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7
            .byte $77, $77, $77, $77, $77, $77, $77, $77
            .byte $F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7
            .byte $77, $77, $77, $77, $77, $77, $77, $77
            .byte $F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7
                  
YHiresHA:   ; high byte of address of each hires line on page $20
            .byte $20, $24, $28, $2C, $30, $34, $38, $3C
            .byte $20, $24, $28, $2C, $30, $34, $38, $3C
            .byte $21, $25, $29, $2D, $31, $35, $39, $3D
            .byte $21, $25, $29, $2D, $31, $35, $39, $3D
            .byte $22, $26, $2A, $2E, $32, $36, $3A, $3E
            .byte $22, $26, $2A, $2E, $32, $36, $3A, $3E
            .byte $23, $27, $2B, $2F, $33, $37, $3B, $3F
            .byte $23, $27, $2B, $2F, $33, $37, $3B, $3F
            .byte $20, $24, $28, $2C, $30, $34, $38, $3C
            .byte $20, $24, $28, $2C, $30, $34, $38, $3C
            .byte $21, $25, $29, $2D, $31, $35, $39, $3D
            .byte $21, $25, $29, $2D, $31, $35, $39, $3D
            .byte $22, $26, $2A, $2E, $32, $36, $3A, $3E
            .byte $22, $26, $2A, $2E, $32, $36, $3A, $3E
            .byte $23, $27, $2B, $2F, $33, $37, $3B, $3F
            .byte $23, $27, $2B, $2F, $33, $37, $3B, $3F
            .byte $20, $24, $28, $2C, $30, $34, $38, $3C
            .byte $20, $24, $28, $2C, $30, $34, $38, $3C
            .byte $21, $25, $29, $2D, $31, $35, $39, $3D
            .byte $21, $25, $29, $2D, $31, $35, $39, $3D
            .byte $22, $26, $2A, $2E, $32, $36, $3A, $3E
            .byte $22, $26, $2A, $2E, $32, $36, $3A, $3E
            .byte $23, $27, $2B, $2F, $33, $37, $3B, $3F
            .byte $23, $27, $2B, $2F, $33, $37, $3B, $3F


YLoresL:    ; low byte of start of text line
            .byte $00, $80, $00, $80, $00, $80, $00, $80
            .byte $28, $A8, $28, $A8, $28, $A8, $28, $A8
            .byte $50, $D0, $50, $D0, $50, $D0, $50, $D0

YLoresS:    ; low byte of end of text line (for stack)
            .byte $27, $A7, $00, $A7, $27, $A7, $27, $A7
            .byte $4F, $CF, $4F, $CF, $4F, $CF, $4F, $CF
            .byte $77, $F7, $77, $F7, $77, $F7, $77, $F7

YLoresHA:   ; high byte of start of text line in $400 page
            .byte $04, $04, $05, $05, $06, $06, $07, $07
            .byte $04, $04, $05, $05, $06, $06, $07, $07
            .byte $04, $04, $05, $05, $06, $06, $07, $07

; I took DOOM's "random" numbers.
; https://github.com/id-Software/DOOM/blob/77735c3ff0772609e9c8d29e3ce2ab42ff54d20b/linuxdoom-1.10/m_random.c#L27
; Good enough.
Random:
            .byte 0,   8, 109, 220, 222, 241, 149, 107,  75, 248, 254, 140,  16,  66
            .byte 74,  21, 211,  47,  80, 242, 154,  27, 205, 128, 161,  89,  77,  36
            .byte 95, 110,  85,  48, 212, 140, 211, 249,  22,  79, 200,  50,  28, 188
            .byte 52, 140, 202, 120,  68, 145,  62,  70, 184, 190,  91, 197, 152, 224
            .byte 149, 104,  25, 178, 252, 182, 202, 182, 141, 197,   4,  81, 181, 242
            .byte 145,  42,  39, 227, 156, 198, 225, 193, 219,  93, 122, 175, 249,   0
            .byte 175, 143,  70, 239,  46, 246, 163,  53, 163, 109, 168, 135,   2, 235
            .byte 25,  92,  20, 145, 138,  77,  69, 166,  78, 176, 173, 212, 166, 113
            .byte 94, 161,  41,  50, 239,  49, 111, 164,  70,  60,   2,  37, 171,  75
            .byte 136, 156,  11,  56,  42, 146, 138, 229,  73, 146,  77,  61,  98, 196
            .byte 135, 106,  63, 197, 195,  86,  96, 203, 113, 101, 170, 247, 181, 113
            .byte 80, 250, 108,   7, 255, 237, 129, 226,  79, 107, 112, 166, 103, 241
            .byte 24, 223, 239, 120, 198,  58,  60,  82, 128,   3, 184,  66, 143, 224
            .byte 145, 224,  81, 206, 163,  45,  63,  90, 168, 114,  59,  33, 159,  95
            .byte 28, 139, 123,  98, 125, 196,  15,  70, 194, 253,  54,  14, 109, 226
            .byte 71,  17, 161,  93, 186,  87, 244, 138,  20,  52, 123, 251,  26,  36
            .byte 17,  46,  52, 231, 232,  76,  31, 221,  84,  37, 216, 165, 212, 106
            .byte 197, 242,  98,  43,  39, 175, 254, 145, 190,  84, 118, 222, 187, 136
            .byte 120, 163, 236, 249
