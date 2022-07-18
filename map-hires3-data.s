; tables associated with map-hires3.s

; table of map x-coordinates and the corresponding place to find them in a line, for use
; when we selectively update the screen display.  Indexed by x-coordinate in map row.

; Lookup for ZCurrMapX values for updsingle
MapEnds:    .byte   6, 6, 6, 6, 6, 6, 6
            .byte   13, 13, 13, 13, 13, 13, 13
            .byte   20, 20, 20, 20, 20, 20, 20
            .byte   27, 27, 27, 27, 27, 27, 27
            .byte   34, 34, 34, 34, 34, 34, 34
            .byte   41, 41, 41, 41, 41, 41, 41
            .byte   48, 48, 48, 48, 48, 48, 48
            .byte   55, 55, 55, 55, 55, 55, 55
            .byte   62, 62, 62, 62, 62, 62, 62

; Lookup for ZCurrDrawX values for updsingle
MapPixG:    .byte   2, 2, 2, 2, 2, 2, 2
            .byte   6, 6, 6, 6, 6, 6, 6
            .byte   10, 10, 10, 10, 10, 10, 10
            .byte   14, 14, 14, 14, 14, 14, 14
            .byte   18, 18, 18, 18, 18, 18, 18
            .byte   22, 22, 22, 22, 22, 22, 22
            .byte   26, 26, 26, 26, 26, 26, 26
            .byte   30, 30, 30, 30, 30, 30, 30
            .byte   34, 34, 34, 34, 34, 34, 34
