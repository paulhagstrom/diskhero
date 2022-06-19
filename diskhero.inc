; definitions

; i/o
KBD         = $C000     ; keyboard
KBDFLAG     = $C008     ; keyboard flag
KBDCLEAR    = $C010     ; keyboard strobe
BEEP        = $C040     ; beep
SPEAKER     = $C030     ; click
; graphics modes
GRAPHICS    = $C050     ; "clear text mode"
TEXT        = $C051     ; set text mode
NOMIX       = $C052     ; "clear mix mode"
MIX         = $C053     ; "set mix mode"
PAGEONE     = $C054     ; "clear PG2 mode"
PAGETWO     = $C055     ; PG2 mode
LORES       = $C056     ; "clear hires mode"
HIRES       = $C057     ; set hires mode
; smooth scrolling
SCROLLON    = $C0D9     ; enable smooth scroll (vertical shift)
SCROLLOFF   = $C0D8     ; disable smooth scroll
LC000       = $C000     ; used as base for slow scroll offset
LC002       = $C002
LC004       = $C004
; graphics buffers
TEXTBUFA    = $0400     ; line 0
TEXTBUFB    = $0480     ; line 1
TEXTBUFC    = $0500     ; line 2
TEXTBUFD    = $0580     ; line 3
TEXTBUFE    = $0600     ; line 4
TEXTBUFF    = $0680     ; line 5
GRBUFA      = $2000     ; start of page 1
; VIAs
E_IORB      = $FFE0     ; includes HBL?
D_ACR       = $FFDB     ; ffdx auxiliary control register
E_ACR       = $FFEB     ; ffex auxiliary control register
D_PCR       = $FFDC     ; ffdx peripheral control register
E_PCR       = $FFEC     ; ffex peripheral control register
D_IFR       = $FFDD     ; ffdx interrupt flag register
E_IFR       = $FFED     ; ffex interrupt flag register
D_IER       = $FFDE     ; ffdx interrupt enable register
E_IER       = $FFEE     ; ffex interrupt enable register
EReg        = $FFDF     ; Environment register
Bank_Reg    = $FFEF     ; Bank register
IRQVECT     = $ffcd     ; Monitor IRQ points here
Z_REG       = $ffd0     ;zero page register
CLOCK       = $c070     ; real time clock

;SOS Calls
;REQUEST_SEG    = $40
;RELEASE_SEG    = $45
TERMINATE   = $65
;OPEN        = $C8
;READ        = $CA
;CLOSE        = $CC

; indirect addressing
Ptr1        = $20
Ptr2        = $22
CExtPG      = $1601          ; Interp extended address offset
Numbor      = $24
