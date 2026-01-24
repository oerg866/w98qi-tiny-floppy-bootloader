; Tiny Linux Bootloader
; (c) 2014- Dr Gareth Owen (www.ghowen.me). All rights reserved.
; Some code adapted from Sebastian Plotz - rewritten, adding pmode and initrd support.
; Further adaptations for WIN98QI by Eric Voirin

;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.


[BITS 16]
org 0x7c00

    ;;; the whole opening preamble enables "unreal mode" -- for more info, see
    ;;; https://wiki.osdev.org/Unreal_Mode
entry:
    cli
    xor ax, ax
    mov ds, ax
    mov ss, ax
    mov sp, 0x7c00          ; setup stack 

    ; we load the 2nd sector to 0x7e00 so we can overwrite 7c00 with another boot sector if 
    ; the boot prompt is skipped
    
;   Original code, optimized for size a little
;    mov ax, 0x0001 ; count
;    mov bx, 0x7E00 ; offset
;    mov cx, 0x0000 ; segment

    mov cx, ax
    inc ax
    mov bx, 0x7E00
    call flopread

; This check asks the user if he wants to boot our floppy/CDROM and if it times out then we
; attempt to boot from a hard disk.
;
; ENTER   = Boot normally
; F1 - F4 = Certain levels of DMA dis/enablement
bootCheck:
    mov si, newLineStr
    call print

    mov si, bootStr
    call print

    ; Get initial tick value
    call getCurrentTick

    mov di, lastSystemTime
    mov dword [di], ebx

.waitEnterLoop:
    ; Print (skipping in X), the carriage return will place the cursor back on the start of the line
    mov si, skipStr
    call print

    ; check keyboard buffer
    mov ah, 0x01        ; check for keystroke
    int 0x16
    jz .delay1Second    ; No key was pressed

    ; Get and discard the keypress
    ; This is necessary so we can accept further keys
    ; if the originally pressed key was not for us.
    xor ah, ah
    int 16h             

    ; Set up LibATA Flags!
    ; F1 = libata.dma=0     Disable all PATA and SATA DMA
    ; F2 = libata.dma=1     PATA and SATA Disk DMA only
    ; F3 = libata.dma=2     ATAPI (CDROM) DMA only
    ; F4 = libata.dma=4

    mov si, cmdLineLibataDmaValue
    mov byte [si], 0x08  

    cmp ah, 0x3B           ; F1
    je short .f1
    cmp ah, 0x3C           ; F2
    je short .f2
    cmp ah, 0x3D           ; F3
    je short .f3
    cmp ah, 0x3E           ; F4
    je short .f4
    cmp ah, 0x1C            ; Enter
    je short .enter

.delay1Second:
    call getCurrentTick

    ; No check for midnight rollover - need to save bytes...
    ; Check if this is a second's worth of difference.
    mov ecx, dword [di]
    mov eax, ebx
    sub eax, ecx
    cmp eax, 18 ; It's actually 18.2 clocks per second but good enough...
    jl .waitEnterLoop ; Second hasnt elapsed yet, back to the loop

    mov dword [di], ebx
    mov al, byte [bootTimerValue]
    dec al
    cmp al, 0x30 ; '0' means we've timed out
    jz exitAndBootFromDisk
    mov byte [bootTimerValue], al

    jmp .waitEnterLoop

; F-Keys - starts with 8, shifts right by F count.
; 
.f1:    shr byte [si], 1    ; 8, 4, 2, 1, 0
.f2:    shr byte [si], 1    ; 8, 4, 2, 1
.f3:    shr byte [si], 1    ; 8, 4, 2
.f4:    shr byte [si], 1    ; 8, 4
    ; Turn into ASCII number character
    add byte [si], 0x30
    ; Make libata be part of the string by overwriting null terminator with a space
    mov byte [cmdLineDefualtEnd], ' ' 
.enter:
    ; User pressed a supported key. We can now resume booting.
    mov si, newLineStr
    call print

    ; Print kernel command line, useful for diagnosis (I guess)
    mov si, cmdLine
    call print

    ; Ugh this is wasteful, but no clue how to make it better...
    mov si, newLineStr
    call print

    mov byte [errStr], 0x3F         ; Place error number into error string

    ; enable a20 gate and error out if unsuccessful
    call enableA20Gate

    ; was a20 enablement successful? if not, print error and die
    jz errPrint

    ; now get into protected move (32bit) as kernel is large and has to be loaded high

    lgdt [gdt_desc] ; load global descriptor table
    mov eax, cr0
    or eax, 1
    mov cr0, eax ; enables protected mode

    jmp $+2

    mov bx, 0x8 ; first descriptor in GDT
    mov ds, bx
    mov es, bx
    mov gs, bx

    and al, 0xFE ; back to real mode
    mov cr0, eax
    
    xor ax,ax ; restore segment values - now limits are removed but seg regs still work as normal
    mov ds, ax
    mov gs, ax
    mov cx, 0x1000 ; segment for kernel load (mem off 0x10000)
    mov es, cx
    sti

    ; now in UNREAL mode
    
    mov bx, ax ; ax is 0 right now
    inc al ; one sector
    ; xor bx,bx ; offset
    ; mov cx, 0x1000 ; seg cx is already 0x1000 here.
    ; copy the first 512 bytes of the image in so we can read its size
    call flopread 

read_kernel_setup:
    ; Useful reading:
    ; https://www.kernel.org/doc/Documentation/x86/boot.txt
    mov si, [es:0x1f0]          ; no of sectors (see: linux/x86 boot protocol)
    shr si, 8                   ; is 0?
    jne read_kernel_setup.next  ; if so, don't set default value
    mov si, 4                   ; default is 4 
.next:
    ; now read the rest of the real mode kernel header in
    ; ax = count, bx = offset, cx = segment
    mov bx, 512
.loop:
    mov cx, 0x1000 ; segment
    call flopread
    ;add bx, 512 ; next offset
    add bh, 2
    dec si
    jnz .loop

    ; We assume the kernel being run is new enough to save bytes...
    ; cmp word [es:0x206], 0x204 ; require protocol 2.04+ 
    ; jb errKernel
    ; We build our own kernels which we know work like this
    ; test byte [es:0x211], 1 ; make sure the real mode loader _actually_ wants to be at 0x10000
    ; jz errKernel

    mov byte [es:0x210], 0xe1 ;loader type we set this ourselves
    mov byte [es:0x211], 0x81 ;heap use? !! SET Bit5 to Make Kern Quiet
    mov word [es:0x224], 0xde00 ;head_end_ptr
    mov byte [es:0x227], 0x01 ;ext_loader_type / bootloader id
    mov dword [es:0x228], 0x1e000 ;cmd line ptr

    ; copy cmd line 
    mov si, cmdLine
    mov di, 0xe000 
    mov cx, cmdLineLen
    rep movsb ; copies from DS:si to ES:di (0x1e000)

loader:
    ; modern kernels are bzImage ones (despite name on disk and so
    ; the protected mode part must be loaded at 0x100000
    ; load nSectorsPerStep sectors at a time to 0x2000, then copy to 0x100000

    mov edx, [es:0x1f4] ; bytes to load
    shl edx, 4 ; (note that edx is initially loaded with size in "16-byte paras")

    mov dword [totalSizeBytes], edx ; save for later

    ; Prepare edi for movement to high memory. 
    mov edi, 0x100000   ; destintation addr

    ; We read a couple of sectors before this so the first value of sectors to read needs to be aligned to a nSectorsPerStep boundary
    ; initialRead = nSectorsPerStep - (flSect mod nSectorsPerStep)
.alignSectorCount:
    nSectorsPerStep equ nSectorsPerTrackDef ; For a regular floppy disk, we read 18 sectors at a time.
    push dx
    xor eax, eax
    mov dx, ax
    mov ax, word [flSect]

    xor dx, dx
    mov bx, nSectorsPerStep
    div bx
    mov bx, dx ; bx = remainder = flSect mod nSectorsPerStep

    mov ax, nSectorsPerStep ; Subtract from nSectorsPerStep
    sub ax, bx

    shl ax, 9  ; To get bytes from sectors
    pop dx

    jmp loader.initialSkip

.loop:

    mov ax, nSectorsPerStep*512

.initialSkip:
    cmp eax, edx
    jbe loader.moreThanNSectorsLeft

    mov ax, dx              ; We have less than nSectorsPerStep*512 bytes left, adjust
    ;add ax, 512             ; If remainder is less than a sector's size we need to read it anyway, garbage data at the end doesn't matter
    add ah, 2

.moreThanNSectorsLeft:
    pusha
    shr ax, 9              ; Get sector count from bytes

    xor bx, bx              ; offset
    mov cx, 0x2000          ; segment
    call flopread           ; Read from floppy
    popa

    ; If we end up here, the read worked and we can move the data to high memory

    ; source = 0x20000
    ; dst = highmove_addr (initially 0x100000), counting along with us
.highmove:
    mov cx, ax              ; counter in bytes
    mov esi, 0x20000        ; source addr
.moveloop:
    mov ebx, [ds:esi]
    mov [ds:edi], ebx
    inc si                  ; Copying bytes instead of DWORDs to save some bytes...
    inc edi
    loop .moveloop          ; for loop instruction, ecx is the counter

    ; Data moved
    call printProgress
    sub edx, eax            ; Subtract the amount of bytes we just read
    jns loader.loop         ; If we're not done yet, go back (jns = jump if not sign, if we read garbage data we subtract and the number becomes negative)

;    ; All data loaded and moved, start the kernel
;    xor edx, edx
;    call printProgress


kernel_start:
    cli
    mov ax, 0x1000
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax
    mov sp, 0xe000
    jmp 0x1020:0

; Floppy read function
; ax -- number of sectors count (we save eax)
; bx -- offset (possibly buggy on bad bioses, keep low or zero) (clob)
; (should be fine as long as the real mode header isn't ungodly large)
; cx -- segment address (clobberable)
flopread:
    pushad
    push es

    ; al is sector read count
    ; cx is cylinder and sector,  dh is head, dl is drive
    ; es:bx is buffer address ptr 
    mov es, cx ; segment is constant in this operation

    ; convert sectors to CHS
    ; adapted from kolibrios 
    ; (https://github.com/Harmon758/kolibrios/blob/master/kernel/trunk/bootloader/boot_fat12.asm)
    ; sector number = (flSect % sectors_per_track) + 1
    ; pre.track number = (flSect/ sectors_per_track)
    ; head number = pre.track number % number of heads
    ; track number = pre.track number / number of heads

    push bx

    push ax
    mov ax, word [flSect]
    mov bx, nSectorsPerTrackDef
    xor dx, dx
    div bx
    inc dx ; 
    mov cl, dl  ; cl -- sector number
    mov bx, 0x2 ; num heads fixed to 2
    xor dx, dx
    div  bx
    ; !!!!!!! ax = track number, dx = head number
    mov ch, al          ; ch=track number
    xchg dh, dl         ; dh=head number
    pop ax

    ; Increase sector counter
    add word [flSect], ax


    ; so cx has an absolutely Cursed layout
    ; (see http://www.techhelpmanual.com/188-int_13h_02h__read_sectors.html)
    ; so we need to do some Funky Things 
    ; (thank you ibm i hate this)
    ; (this approach does not suppoprt more than 255 cylinders)
    mov bl, cl ; temp move sector number to bl
    and bl, 0x3f ; lop off the top 2 bits
    and cl, 0xc0 ; lop off the bottom 6 bits 
    or cl, bl ; add the sector number back in
    pop bx

    xor dl, dl      ; read from first floppy -- this may not be necessary
    mov ah, 0x02    ; action = read

    mov si, 20 ; set floppy retry counter

flopread.retry:
    push ax
    int 0x13
    pop ax

    jnc flopread.success

    dec si
    jz errRead

    jmp flopread.retry

flopread.success:
    pop es
    popad
    ret

; This must be in this sector because it is used in flopread :) 
flSect dw 1 ; start sector (this is zero-indexed)

; Get current system tick using  int 0x1a
; Returns midnight flag in al and 32 bit tick value in ebx
getCurrentTick:
    xor ax, ax
    int 0x1a

    mov bx, cx  ; Make 32-bit value ... easier
    shl ebx, 16
    mov bx, dx
    ret

;boot sector magic
    times   510-($-$$)  db  0
    dw  0xaa55

; ----------------------------------------------------------------
;
; SECOND SECTOR STARTS HERE
;
Sector2:

; Print read error
; Should be in sector 1 but ran out of space :(
errRead:
    rol al, 4
    call printHexDigit
    rol al, 4
    call printHexDigit

; Print error and hang
errPrint:
    mov si, errStr
    call print
    jmp $

; Print progress on the screen
; edx = bytes left to read
printProgress:
    pushad

    ; What we want is Load: [................]
    ; so basically 16 dots = 100%
    ;               0 dots = 0%

    mov eax, dword [totalSizeBytes]
    mov ebx, eax    ; EBX = total size
    sub eax, edx    ; EAX = amount of bytes read = total size - bytes left
    shl eax, 4      ; EAX = amount of bytes read * 16
    xor edx, edx    ; clear EDX so the division isnt disturbed
    div ebx         ; EAX = (bytes read * 16) / (total size)
    mov cx, ax
    inc cx          ; Make sure it's not 0

    mov al, cl
    out 0x80, al

    lea si, progressBar
.makeDots:
    mov byte [si], '.'
    inc si
    loop .makeDots

    lea si, progressStr
    call print

    popad
    ret

; Prints a hex byte in al onto the screen
printHexDigit:
    push ax
    and al, 0x0f
    add al, '0'          ; Convert to ASCII ('0'-'9' for 0-9)
    cmp al, '9'          ; Check if it's greater than '9'
    jbe short printHexDigit.noHexAdj ; If <= '9', it's done
    add al, 7            ; Adjust to 'A'-'F' for 10-15
printHexDigit.noHexAdj:
    mov ah, 0x0E         ; BIOS teletype function
    int 0x10             ; Call BIOS interrupt
    pop ax
    ret

; Print a null terminated string on the screen
; si = source str
print:
    lodsb
    test al, al
    jz print.end
    mov ah, 0x0E         ; BIOS teletype function
    int 0x10             ; Call BIOS interrupt
    jmp print
print.end:
    ret

;   ----------------------------------------------
;   A20 GATE RELATED CODE
;   ----------------------------------------------

; Jumptable for different methods to enable A20 gate
enableA20GateMethods:
    dw A20_BIOS ; via BIOS call
    dw A20_KBC  ; via KBC
    dw A20_fast ; via Port 0x92

A20MethodCount equ 3

; A20 method via BIOS call
A20_BIOS:
    mov ax, 0x2403      ; Check if this method is supported first (else it will reboot!!!)
    int 0x15

    jc .biosa20_skip    ; Get out if INT 15h is not supported (carry flag set)
    test ah, ah
    jnz .biosa20_skip

    mov ax, 0x2401      ; INT 15h is supported, now try it
    int 0x15
.biosa20_skip:
    ret

; A20 method via KBC
A20_KBC:
    mov al, 0xd1
    call writeKBCCommand

    mov al, 0xdf
    call writeKBCData

    mov al, 0xff
    call waitForKBC    
    ret

; Fast A20 Method via Port 0x92
A20_fast:
    in al, 0x92
    or al, 2
    and al, 0xfe    ; mask out reset bit!!
    out 0x92,al
    ret

; Enable A20 Gate
; Zero flag set = error
; Zero flag clear = successful
enableA20Gate:
    mov si, enableA20GateMethods
    mov cx, A20MethodCount
.a20loop:
    call word [si]
    inc si
    inc si
    call checkA20   ; is A20 gate actually enabled?
    jnz .exit       ; if yes, exit, the flag will still be valid for caller
    loop .a20loop   ; Next method
.exit:
    ret

; Macro to delay execution by a few cycles
%macro IO_DELAY 0
    xor ax, ax
    out 0x80, al
%endmacro

; Write data in al to Keyboard controller
writeKBCData:
    out 0x60, al
    jmp short waitForKBC

; Write command in al to Keyboard controller
writeKBCCommand:
    out 0x64, al


; Waits for Keyboard Controller to be ready
waitForKBC:
    pusha
    mov cx, 0xFFFF
.loop:
    IO_DELAY
    in al, 0x64
    
    ; Check if something is waiting in the keyboard buffer
    ; if not, continue our check, else fetch it and reiterate
    test al, 0x01
    jz .nothingPendingCheckBuffersEmpty

    ; There is something in the buffer, fetch it
    IO_DELAY
    in al, 0x60
    loop .loop

.nothingPendingCheckBuffersEmpty:
; Nothing in the buffer, check if status indicates so
    test al, 0x02
    jz .exit
    loop .loop

.exit:
; Keyboard controller is now flushed!
    popa
    ret

; Checks the status of the a20 line.
; Returns: Zero flag set if the a20 line is disabled (memory wraps around)
;          Zero flag cleared if the a20 line is enabled (memory does not wrap around)
; Code from OSDev Wiki (Public Domain Licensed) - I'm too dumb for this...!
checkA20:
    ; Compare boot sector identifier at 0x7C00 + 254 bytes to 1MB higher
    ; i.e. ffff:7e0e
    ; if they are NOT identical, A20 IS ENABLED!
    pusha
    push ds
    xor ax, ax ; ax = 0
    mov es, ax
    mov di, 0x7DFE

;    mov ax, 0xffff
    not ax
    mov ds, ax
    mov si, 0x7E0E

    ; We jump to exit without doing anything to AX
    ; Since ax is nonzero, the function will exit with zero flag cleared
    ; -> successful

    ; Test 1: ds:si has the signature? that's a problem.
    mov bx, word [es:di]
    cmp word [ds:si], bx
    
    ; they differ, get out
    jne .checkA20.exit

    ; Signatures match but it may be a coincidence. To be sure:
    ; Test 2: Invert the value in es:di and check if ds:si still matches.
    not word [es:di]

    mov bx, word [es:di]
    cmp word [ds:si], bx
    jne .checkA20.exit

    ; Failure, clear ax so that the zero flag will be set.
    xor ax, ax

.checkA20.exit:
    pop ds
    test ax, ax
    popa
    ret

; Exits the bootloader, shuts down any cd floppy emulation
; and attempts to find and load a HDD boot sector.
exitAndBootFromDisk:
    ; Reset CD-ROM floppy emulation if active
    mov ax, 0x4B00
    xor dx, dx
    mov si, 0x8000          ; "Empty" Specification packet... Just need somewhere for the interrupt to write to.
    mov byte [si], 0x13     ; Start of the packet needs to be 0x13 (length in bytes)
    int 0x13

    ; Reset disk system (why?)
    xor ax, ax
    int 0x13         ; DISK - RESET DISK SYSTEM

    mov dl, 0x80
.bootAttemptLoop:
    ; Load HDD boot sector.
    xor ax, ax
    mov es, ax
    mov bx, 0x7C00  ; target buffer 
    mov dh, ah      ; Head 0
    mov ax, 0x0201  ; Read, 1 sector
    mov cx, 1       ; Track 0, Sector 1
    int 0x13        ; DISK - READ SECTORS INTO MEMORY

    ; if carry is set, we have an error condition and could not read.
    jc .bootAttemptSetNextDrive

    ; check for 0xaa55 signature
    cmp word [0x7DFE], 0xAA55 ; remember this is byteswapped, in the sector it's 55 aa
    jne .bootAttemptSetNextDrive

    ; Attempt to boot the boot sector. Normally you'd restore DS and ES here but we already did that.
    jmp 0x7C00

.bootAttemptSetNextDrive:
    inc dl
    cmp dl, 0xA0    ; I have no idea what the upper limit of this is
    jne .bootAttemptLoop

.bootError:
    int 0x18        ; Should yield a "PREESS A KEY TO REBOOT" screen.

                
;    db '                                                              '
; All data lives here...
%include "data.inc"

    times   1024-($-$$) db 0
