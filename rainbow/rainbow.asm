    processor 6502

    include "../include/vcs.h"
    include "../include/macro.h"

    seg code 
    org $F000

Start: 
    CLEAN_START                 ; Macro para limpar a memoria e o TIA com segurança 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comeca um novo frame ligando VBLANK e VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NextFrame:
    lda #2                      ; mesmo que valor binario %00000010
    sta VBLANK                  ; liga VBLANK
    sta VSYNC                   ; liga VSYNC 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gera tres linhas de VSYNC 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    sta WSYNC                   ; primeiro scanline
    sta WSYNC                   ; 2nd scanline
    sta WSYNC                   ; 3rd scanline 

    lda #0
    sta VSYNC                   ; desliga VSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deixa o TIA output as 37 scanlines de VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ldx #37                     ; X=37  (contar ate 37 scanlines)
LoopVBlank:
    sta WSYNC                   ; atinge WSYNC e espera pelo proximo scanline 
    dex                         ; decrementa o contador X
    bne LoopVBlank              ; Loop while X != 0

    lda #0
    sta VBLANK                  ; desliga VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Desenha 192 scanlines visiveis (kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ldx #192                    ; contador das 192 scanlines visiveis 
LoopVisible: 
    stx COLUBK                  ; seta a background color
    sta WSYNC                   ; espera pelo proximo scanline
    dex                         ; X--
    bne LoopVisible             ;  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output mais 30 linhas VBLANK para completar o frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2                      ; 
    sta VBLANK                  ; atinge e ativa VBLANK de novo

    ldx #30                     ; contador das 30 linahs de overscan 
LoopOverscan:
    sta WSYNC                   ; espera pela proxima scanline
    dex                         ; X--
    bne LoopOverscan            ;

    jmp NextFrame               ; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completa o tamanho da ROM até 4Kb 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC
    .word Start
    .word Start



