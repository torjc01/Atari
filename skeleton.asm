    processor 6502
    seg Code            ; Define a new segment called Code
    org $F000           ; Define the origin of ROM at memory address $F000
Start:
    ; TODO ; 

    org $FFFC           ; End the ROM by adding required values to memory position $FFFC
    .word Start         ; Put 2 bytes with the reset address at memory position $FFFC
    .word Start         ; Put 2 bytes with the reset address at memory position $FFFE
