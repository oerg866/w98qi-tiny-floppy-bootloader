
;-----------------------------------------------------------------------------
; DATA
; Strings, TC, ETC.

;descriptor
gdt_desc:
    dw gdt_end - gdt - 1
    dd gdt

; access byte: [present, priv[2] (0=highest), 1, Execbit, Direction=0, rw=1, accessed=0] 
; flags: Granuality (0=limitinbytes, 1=limitin4kbs), Sz= [0=16bit, 1=32bit], 0, 0
gdt:
                        dq 0 ; first entry 0
                        ;flat data segment
                        dw 0FFFFh ; limit[0:15] (aka 4gb)
                        dw 0      ; base[0:15]
                        db 0      ; base[16:23]
                        db 10010010b  ; access byte 
                        db 11001111b    ; [7..4]= flags [3..0] = limit[16:19]
                        db 0 ; base[24:31]
gdt_end:

; config options
cmdLine:                db 'tsc=unstable noapic acpi=off'
cmdLineDefualtEnd:      db 0
                        db 'libata.dma='
cmdLineLibataDmaValue:  db '0',0
cmdLineLen:             equ $-cmdLine

; Progress string and value for printing - putting this here because sector 2 is quite full now
progressStr:            db 'Load: ['
progressBar:            db '                 ]',
progressCarriageReturn: db 0x0d, 0

newLineStr:             db 0xd, 0xa, 0
bootStr:                db 'Press ENTER to start QuickInstall', 0x0d, 0x0a, 0x0a
                        db '[DMA Debug |F1: None|F2: HDD|F3: CD/DVD|F4: CF]', 0x0d, 0x0a
skipStr:                db 'Skipping in '
bootTimerValue:         db '4', 0x0d, 0 ; the 0x0d means we go back to the start of the line so we can update the counter

errStr                  db ' ERR',0 ; Save one byte 

totalSizeBytes:         ; These are used at different times so they share a memory location.
lastSystemTime:         dd 0
