    processor 6502

    include "../include/vcs.h"
    include "../include/macro.h"

    seg code            ; Define a new segment called Code
    org $F000           ; Define the origin of ROM at memory address $F000

START:
    ; CLEAN_START         ; Macro to safely clear the memory

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Set background luminosity color to yellow 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #$1E            ; Load color into A ($1E is NTSC yellow)
    sta COLUBK          ; Store A to backgroundColor address $09

    jmp START           ; Repeat from START


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fill ROM size to exactly 4Kb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC           ; End the ROM by adding required values to memory position $FFFC
    .word START         ; Put 2 bytes with the reset address at memory position $FFFC
    .word START         ; Put 2 bytes with the reset address at memory position $FFFE
