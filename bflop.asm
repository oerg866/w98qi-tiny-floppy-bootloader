; Tiny Linux Bootloader
; (c) 2014- Dr Gareth Owen (www.ghowen.me). All rights reserved.
; Some code adapted from Sebastian Plotz - rewritten, adding pmode and initrd support.

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

; Useful links:

%include "config.inc"

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
    
    mov ax, 0x0001 ; count
    mov bx, 0x7E00 ; offset
    mov cx, 0x0000 ; segment
    call flopread

    call bootCheck

    ; enable a20 gate and error out if unsuccessful

    call enableA20Gate
    or ax, ax
    jz errA20

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
    mov al, [es:0x1f1] ; no of sectors (see: linux/x86 boot protocol)
    cmp ax, 0
    jne read_kernel_setup.next
    mov ax, 4 ; default is 4 

.next:
    ; now read the rest of the real mode kernel header in
    ; ax = count
    mov bx, 512 ; next offset
    mov cx, 0x1000 ; segment
    call flopread

    ; We assume the kernel being run is new enough to save bytes...
    ; cmp word [es:0x206], 0x204 ; require protocol 2.04+ 
    ; jb errKernel
    test byte [es:0x211], 1 ; make sure the real mode loader _actually_ wants to be at 0x10000
    jz errKernel

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

    ; modern kernels are bzImage ones (despite name on disk and so
    ; the protected mode part must be loaded at 0x100000
    ; load 127 sectors at a time to 0x2000, then copy to 0x100000

;load_kernel
    mov edx, [es:0x1f4] ; bytes to load
    shl edx, 4 ; (note that edx is initially loaded with size in "16-byte paras")

; ================= functions ====================
;length in bytes into edx
; uses flopread [hddLBA] and highmove [highmove_addr] vars
;clobbers 0x2000 segment
loader:
.loop:
    cmp edx, 127*512
    jl loader.part_2
    jz loader.finish

    push edx
    mov ax, 127 ;count

    call readFloppyToSeg2000Offset0

    mov si, progStr
    call print
    pop edx
    sub edx, 127*512

    jmp loader.loop

.part_2:   ; load less than 127*512 sectors
    shr edx, 9  ; divide by 512
    inc edx     ; increase by one to get final sector if not multiple - otherwise just load junk - doesn't matter
    mov ax, dx

    call readFloppyToSeg2000Offset0

.finish:

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

; To save more bytes, we put this in a subroutine, as we have two calls like this
readFloppyToSeg2000Offset0:
    xor bx, bx ; offset
    mov cx, 0x2000 ; seg
    call flopread
    call highmove
    ret

highmove_addr dd 0x100000
; source = 0x2000
; count = 127*512  fixed, doesn't if matter we copy junk at end
; don't think we can use rep movsb here as it wont use EDI/ESI in unreal mode
highmove:
    mov esi, 0x20000
    mov edi, [highmove_addr]
    mov edx, 512*127
    mov ecx, 0 ; pointer
.loop:
    mov eax, [ds:esi]
    mov [ds:edi], eax
    add esi, 4
    add edi, 4
    sub edx, 4
    jnz highmove.loop
    mov [highmove_addr], edi
    ret

errStr db 'ER'
progStr db '.',0 ; Save one byte 

errA20: mov byte [progStr], 0x30        ; Replace dot with the error number
    jmp errPrint
errRead: mov byte [progStr], 0x31       ; Replace dot with the error number
    jmp errPrint
errKernel: mov byte [progStr], 0x32     ; Replace dot with the error number
errPrint:
    mov si, errStr
    call print
    jmp $
    

flSect dw 1 ; start sector (this is zero-indexed)
flopread:
    push eax
    push es
    ; ax -- number of sectors count (we save eax)
    ; bx -- offset (possibly buggy on bad bioses, keep low or zero) (clob)
    ; (should be fine as long as the real mode header isn't ungodly large)
    ; cx -- segment address (clobberable)


    ; al is sector read count
    ; cx is cylinder and sector,  dh is head, dl is drive
    ; es:bx is buffer address ptr 
    mov es, cx ; segment is constant in this operation
    mov si, ax ; si is our sector counter
    
.loop:
; HPC = 2
; SPT = 18
    ; 
    ;
    ; convert sectors to CHS
    ; adapted from kolibrios 
    ; (https://github.com/Harmon758/kolibrios/blob/master/kernel/trunk/bootloader/boot_fat12.asm)
    ; sector number = (flSect % sectors_per_track) + 1
    ; pre.track number = (flSect/ sectors_per_track)
    ; head number = pre.track number % number of heads
    ; track number = pre.track number / number of heads
    push bx
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
    pop bx



    ; so cx has an absolutely Cursed layout
    ; (see http://www.techhelpmanual.com/188-int_13h_02h__read_sectors.html)
    ; so we need to do some Funky Things 
    ; (thank you ibm i hate this)
    ; (this approach does not suppoprt more than 255 cylinders)
    mov al, cl ; temp move sector number to al
    and al, 0x3f ; lop off the top 2 bits
    and cl, 0xc0 ; lop off the bottom 6 bits 
    or cl, al ; add the sector number back in


    xor dl, dl ; read from first floppy -- this may not be necessary
    mov ax, 0x0201 ; read 1 sector

    push si
    mov si, 20 ; set floppy retry counter
flopread.retry:
    dec si
    jz errRead
    push ax
    push bx 
    push cx 
    push dx
    int 0x13
    pop dx
    pop cx
    pop bx
    pop ax
    jc flopread.retry
    pop si

    mov ax, word [flSect] ; increment floppy sector counter
    inc ax
    mov [flSect], ax

    add bx, 512 ; increment offset
    dec si ; decrement number of sects to read

    
    jnz flopread.loop
flopread.finish:
    pop es
    pop eax
    ret


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
    cmdLine db cmdLineDef,0
    cmdLineLen equ $-cmdLine
    ;initRdSize dd initRdSizeDef ; from config.inc
    ;hddLBA db 1   ; start address for kernel - subsequent calls are sequential

    ; see config.inc for more info
    ;nCylinders dw nCylindersPerHeadDef
    ;nSectors db nSectorsPerTrackDef

;boot sector magic
    times   510-($-$$)  db  0
    dw  0xaa55

Sector2:

; This check asks the user if he wants to boot our floppy/CDROM and if it times out then we
; attempt to boot from a hard disk.
bootCheck:
    mov si, newLineStr
    call print

    ; Get initial tick value
    call getCurrentTick
    mov dword [lastSystemTime], ebx

.waitEnterLoop:

    mov si, bootStr
    call print

    ; check keyboard buffer
    mov ah, 0x01 ; chek for keystroke
    int 0x16
    jnz .bootCheckOK

.delay1Second:
    ; I tried to use int 0x15 with ax=0x8600 here but for some reason this is way too fast on 86box...
    ; So I have to use another method.
    ;    mov cx, 0xf
    ;    mov dx, 0x4240
    ;    mov ax, 0x8600
    ;    int 0x15 
    call getCurrentTick

    ; Check for midnight rollover
    test al, al
    jz .noMidnight

    add ebx, 0x1800B0; 0x1800B0 per 24 hrs

.noMidnight:
    ; Check if this is a second's worth of difference.
    mov ecx, dword [lastSystemTime]
    mov eax, ebx
    sub eax, ecx
    cmp eax, 18 ; It's actually 18.2 clocks per second but good enough...
    jl .waitEnterLoop ; Second hasnt elapsed yet, back to the loop

    mov dword [lastSystemTime], ebx
    mov al, byte [bootTimerStr]
    dec al
    cmp al, 0x30 ; '0' means we've timed out
    jz .bootExit
    mov byte [bootTimerStr], al

    jmp .waitEnterLoop

.bootExit:    
    mov ax, 0x4b00
    mov dl, 0
    mov si, 0x7C00  ; We'll overwrite the boot sector anyway, so we tell it to dump stuff here.
    int 13h         ; Bootable CD-ROM - TERMINATE DISK EMULATION
    
    xor ax, ax
    mov dl, 0x80
    int 13h         ; DISK - RESET DISK SYSTEM
    
    mov dl, 0x80    ; Start with first disk in priority list.
.bootAttemptLoop:
    call .loadHDDBootSector

    ; if carry is set, we have an error condition and could not read.
    jc .bootAttemptSetNextDrive

    ; check for 0xaa55 signature
    cmp word [0x7DFE], 0xAA55 ; remember this is byteswapped, in the sector it's 55 aa
    je .bootHDD

.bootAttemptSetNextDrive:
    inc dl
    cmp dl, 0xA0    ; I have no idea what the upper limit of this is
    jne .bootAttemptLoop

.bootError:
    mov si, bootErrStr
    call print
    jmp $

.bootHDD:    
    xor ax, ax
    mov ds, ax
    mov es, ax
    jmp 0x7C00
    

; Try to load a boot sector from a disk to 0x7C00 and boot from it. 
; dl = the drive to boot
; CF is set on read error. Caller should check for 0x55AA signature.
.loadHDDBootSector:
    xor ax, ax
    mov es, ax
    mov bx, 0x7C00  ; target buffer 
    mov ax, 0x0201  ; Read, 1 sector
    mov cx, 1       ; Track 0, Sector 1
    mov dh, 0       ; Head 0
    int 13h         ; DISK - READ SECTORS INTO MEMORY
    retn

.bootCheckOK:
    ; User pressed a key. We can now resume booting.
    mov si, newLineStr
    call print
    ret


newLineStr db 0xd, 0xa, 0
bootStr db ' Press any key to boot Windows 98 QuickInstall ('
bootTimerStr db '4 seconds left)', 0x0d, 0 ; the 0x0d means we go back to the start of the line so we can update the counter
bootErrStr db 0xd, 0xa, ' No bootable Disk found! Halting.', 0

lastSystemTime dd 0

; Get current system tick using  int 0x1a
; Returns midnight flag in al and 32 bit tick value in ebx
getCurrentTick:
    xor ax, ax
    int 0x1a

    mov bx, cx  ; Make 32-bit value ... easier
    shl ebx, 16
    mov bx, dx
    ret

; si = source str
print:
    mov ah, 0xe
print.loop:
    lodsb
    int 0x10
    test al, al
    jnz print.loop
print.end:
    ret

;   ----------------------------------------------
;   A20 GATE RELATED CODE
;   ----------------------------------------------

%macro writeKBCCommand 1
    mov al, %1
    out 0x64, al
%endmacro

%macro writeKBCData 1
    mov al, %1
    out 0x60, al
%endmacro

%macro IO_DELAY 0
    xor ax, ax
    out 0x80, al
%endmacro

%macro jumpIfA20Enabled 1
    call checkA20
    or ax, ax
    jnz %1
%endmacro

; Enables A20 Gate using several methods...
; Returns 1 in ax if enabling was successful.
enableA20Gate:
    jumpIfA20Enabled .exit

.biosa20:               ; Enable A20 using BIOS
   mov ax, 0x2403      ; Check if this method is supported first (else it will reboot!!!)
   int 15h

   jb .biosa20_skip    ; Get out if INT 15h is not supported
   cmp ah, 0
   jnz .biosa20_skip

   mov ax, 0x2401      ; INT 15h is supported, now try it
   int 15h

.biosa20_skip:
    jumpIfA20Enabled .exit

; This method from OSDev sadly reboots a few of my socket 7 machines
; so I removed it. Commented code is kept for reference. Maybe I'm doing
; something wrong?
;
;.kbca20_1:          ; Enable A20 using Keyboard Controller, method 1
;	mov cx, 0x5     ; 5 attempts
;.kbca20_1_loop:
;    writeKBCCommand 0xad  ; Disable keyboard
;    call waitForKBC
;
;    writeKBCCommand 0xd0    ; Command: Read from input
;    call waitForKBC
;
;    in al, 0x60     ; Read the output port
;    push ax         ; Save for later
;    call waitForKBC
;
;    writeKBCCommand 0xd1    ; Command: Write to output
;    call waitForKBC
; 
;    pop ax          ; set bit 2 to enable a20 gate
;    or al, 2
;    out 0x60, al
;    call waitForKBC
;
;    writeKBCCommand 0xae    ; Enable keyboard again
;    call waitForKBC
;
;    jumpIfA20Enabled .exit
;    
;    loop .kbca20_1_loop

.kbca20:                    ; Linux's way to enable A20 via keyboard controller
    call waitForKBC

    writeKBCCommand 0xd1    ; Command write
    call waitForKBC

    writeKBCData 0xdf       ; Command: Enable A20
    call waitForKBC

    writeKBCCommand 0xff    ; dummy command, not sure why? Linux says "UHCI wants it"
    call waitForKBC

    jumpIfA20Enabled .exit

.fasta20:           ; Another method, use Fast A20 on Port 0x92
    in al, 0x92
    or al, 2
    and al, 0xfe    ; mask out reset bit!!
    out 0x92,al

    jumpIfA20Enabled .exit

    ; We end up here with ax being 0, thus meaning unsuccessful :(

.exit:
    ret

; Waits for Keyboard Controller to be ready
waitForKBC:
    push ax
    push cx
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
    pop cx
    pop ax
    ret

; Checks the status of the a20 line.
; Returns: 0 in ax if the a20 line is disabled (memory wraps around)
;          1 in ax if the a20 line is enabled (memory does not wrap around)
; Code from OSDev Wiki (Public Domain Licensed) - I'm too dumb for this...!
checkA20:
    push es
    push ds
    push di
    push si

    xor ax, ax ; ax = 0
    mov es, ax
    mov di, 0x0500

    mov ax, 0xffff
    mov ds, ax
    mov si, 0x0510

    mov al, byte [es:di]
    push ax

    mov al, byte [ds:si]
    push ax

    mov byte [es:di], 0x00
    mov byte [ds:si], 0xFF

    cmp byte [es:di], 0xFF

    pop ax
    mov byte [ds:si], al

    pop ax
    mov byte [es:di], al

    mov ax, 0
    je .exit

    mov ax, 1

.exit:
    pop si
    pop di
    pop ds
    pop es
    ret

    times   1022-($-$$) db 0
    dw 0xaa55