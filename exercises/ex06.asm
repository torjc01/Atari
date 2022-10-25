    processor 6502
    seg Code            ; Define a new segment called Code
    org $F000           ; Define the origin of ROM at memory address $F000
Start:
    ; TODO ; 

                        ; Load the A register with the decimal value 1
                        ; Load the X register with the decimal value 2
                        ; Load the Y register with the decimal value 3
                        ; Increment X
                        ; Increment Y
                        ; Increment A
                        ; Decrement X
                        ; Decrement Y
                        ; Decrement A

    org $FFFC           ; End the ROM by adding required values to memory position $FFFC
    .word Start         ; Put 2 bytes with the reset address at memory position $FFFC
    .word Start         ; Put 2 bytes with the reset address at memory position $FFFE
