    processor 6502
    seg Code            ; Define a new segment called Code
    org $F000           ; Define the origin of ROM at memory address $F000
Start: 
    lda #$82            ; Load the A register with the literal hexadecimal value $82
    ldx #82             ; Load the X register with the literal decimal value 82
    ldy $82             ; Load the Y register with the value that is inside memory position $82
    jmp Start           ; Jump over to start to create an infinite loop

    org $FFFC           ; End the ROM by adding required values to memory position $FFFC
    .word Start         ; Put 2 bytes with the reset address at memory position $FFFC
    .word Start         ; Put 2 bytes with the reset address at memory position $FFFE
