    processor 6502
    seg Code            ; Define a new segment called Code
    org $F000           ; Define the origin of ROM at memory address $F000
Start:
    ; TODO ; 
    lda #100            ; Load the A register with the literal decimal value 100
    adc #5              ; Add the decimal value 5 to the accumulator
    sbc #10             ; Subtract the decimal value 10 from the accumulator
                        ; Register A should now contain the decimal 95 (or $5F in hexadecimal)

    org $FFFC           ; End the ROM by adding required values to memory position $FFFC
    .word Start         ; Put 2 bytes with the reset address at memory position $FFFC
    .word Start         ; Put 2 bytes with the reset address at memory position $FFFE
