    processor 6502
    seg Code            ; Define a new segment called Code
    org $F000           ; Define the origin of ROM at memory address $F000
Start:
    ; TODO ; 
    ldy #10             ; Initialize the Y register with the decimal value 10
Loop:
                        ; TODO:
                        ; Transfer Y to A
                        ; Store the value in A inside memory position $80+Y
                        ; Decrement Y
                        ; Branch back to "Loop" until we are done
    org $FFFC           ; End the ROM by adding required values to memory position $FFFC
    .word Start         ; Put 2 bytes with the reset address at memory position $FFFC
    .word Start         ; Put 2 bytes with the reset address at memory position $FFFE
