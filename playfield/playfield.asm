    processor 6502 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include required header files with defs and macros.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "../include/vcs.h"
    include "../include/macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start ROM code 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg code
    org $F000

Reset:
    CLEAN_START

    ldx #$80                ; Color blue background 
    stx COLUBK

    lda #$1C                ; Color: yellow playfield
    sta COLUPF 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start a new frame by configuring VBLANK and VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:
    lda #02                     ; 
    sta VBLANK                  ; turn VBLANK on  
    sta VSYNC                   ; turn VSYNC on 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate the 3 lines of VSYNC 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    REPEAT 3
        sta WSYNC           ; three scanlines for VSYNC
    REPEND
    lda #0
    sta VSYNC               ; turn VSYNC off 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let the TIA output the 37 lines of VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    REPEAT 37
        sta WSYNC           
    REPEND
    lda #0
    sta VBLANK              ; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the CTRLPF register to allow playfield reflection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ldx #%00000001          ; CTRLPF register (D0 means reflect the PF)
    stx CTRLPF              ; seta o bitmap para o CTRLPF fazer espelhamento

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw the 192 visible scanlines 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ; Skip 7 scanlines with no PF set 
    ldx #%00000000          ; seta a linha vazia para os scanlines 
    stx PF0
    stx PF1                 ; move zeros para todos os campos do PF
    stx PF2

    REPEAT 7
        sta WSYNC           ; 
    REPEND 

    ; Set the PF0 to 1110 (lsb first) and PF1 and PF2 1111-1111
    ldx #%11100000          ; seta o pattern 1110 
    stx PF0                 ; carrega em PF0
    ldx #%11111111          ; seta o pattern 11111111 
    stx PF1                 ; carrega em PF1
    stx PF2                 ; carrega em PF2

    REPEAT 7
        sta WSYNC
    REPEND

    ; Set the next 164 lines with only PF0 third bit enabled
    ldx #%00100000
    stx PF0
    ldx #%00000000
    stx PF1
    stx PF2 

    REPEAT 164
        sta WSYNC
    REPEND

    ; Set the PF0 to 1110 (lsb first) and PF1 and PF2 1111-1111
    ldx #%11100000          ; seta o pattern 1110 
    stx PF0                 ; carrega em PF0
    ldx #%11111111          ; seta o pattern 11111111 
    stx PF1                 ; carrega em PF1
    stx PF2                 ; carrega em PF2

    REPEAT 7
        sta WSYNC
    REPEND

; Skip 7 scanlines with no PF set 
    ldx #%00000000          ; seta a linha vazia para os scanlines 
    stx PF0
    stx PF1                 ; move zeros para todos os campos do PF
    stx PF2

    REPEAT 7
        sta WSYNC           ; 
    REPEND 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output de outras 30 linhas para completar o frame com OVERSCAN 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #02                 ; turn on VBLANK
    sta VBLANK              

    REPEAT 30
        sta WSYNC
    REPEND
    lda #0                  ; turn off VBLANK
    sta VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop para o proximo frame 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp StartFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completa o tamanho da ROM até 4Kb 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC
    .word StartFrame
    .word StartFrame