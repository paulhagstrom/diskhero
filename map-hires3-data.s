; tables associated with map-hires3.s

; table of map x-coordinates and the corresponding place to find them in a line, for use
; when we selectively update the screen display.  Indexed by x-coordinate in map row.

; Bin in which each map x-coordinate finds itself (i.e. division by 7 table)
DivSeven:   .byte   0, 0, 0, 0, 0, 0, 0
            .byte   1, 1, 1, 1, 1, 1, 1
            .byte   2, 2, 2, 2, 2, 2, 2
            .byte   3, 3, 3, 3, 3, 3, 3
            .byte   4, 4, 4, 4, 4, 4, 4
            .byte   5, 5, 5, 5, 5, 5, 5
            .byte   6, 6, 6, 6, 6, 6, 6
            .byte   7, 7, 7, 7, 7, 7, 7
            .byte   8, 8, 8, 8, 8, 8, 8

; last map pixel (0-based) in a given bin
MapEnds:    .byte   6, 13, 20, 27, 34, 41, 48, 55, 62

; first screen pixel in a given bin
MapPixG:    .byte   2, 6, 10, 14, 18, 22, 26, 30, 34

; Location in bank 2 for the 9 update stacks, allows $80 per stack
HRBinLow:   .byte   $00, $80, $00, $80, $00, $80, $00, $80, $00
HRBinHigh:  .byte   $10, $10, $11, $11, $12, $12, $13, $13, $14
            
