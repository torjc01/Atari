    processor 6502
    seg Code            ; Define a new segment called Code
    org $F000           ; Define the origin of ROM at memory address $F000
Start:
    ; TODO ; 

    lda #1              ; Initialize the A register with the decimal value 1
Loop:
                        ; TODO:
                        ; Increment A
                        ; Compare the value in A with the decimal value 10
                        ; Branch back to loop if the comparison was not equals (to zero)

    org $FFFC           ; End the ROM by adding required values to memory position $FFFC
    .word Start         ; Put 2 bytes with the reset address at memory position $FFFC
    .word Start         ; Put 2 bytes with the reset address at memory position $FFFE
