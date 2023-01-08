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

%define DEBUG
%include "config.inc"

[BITS 16]
org	0x7c00

    ;;; the whole opening preamble enables "unreal mode" -- for more info, see
    ;;; https://wiki.osdev.org/Unreal_Mode
	cli
	xor	ax, ax
	mov	ds, ax
	mov	ss, ax
	mov	sp, 0x7c00			; setup stack 

    ; now get into protected move (32bit) as kernel is large and has to be loaded high
    mov ax, 0x2401 ; A20 line enable via BIOS
    int 0x15
    jc err


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
	mov	ds, ax
	mov	gs, ax
    mov ax, 0x1000 ; segment for kernel load (mem off 0x10000)
	mov	es, ax
    sti

    ; now in UNREAL mode

    mov ax, 1 ; one sector
    xor bx,bx ; offset
    mov cx, 0x1000 ; seg
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

    cmp word [es:0x206], 0x204 ; require protocol 2.04+ 
    jb errK
    test byte [es:0x211], 1 ; make sure the real mode loader _actually_ wants to be at 0x10000
    jz errK

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
    call loader

; we disable the initrd code -- if you need initrd, you could try 
; reenabling it (or just embed your initramfs cpio in your kernel i have no idea
; how you crammed a mountable initrd image on a floppy)

;load initrd
;    mov eax, 0x7fab000; this is the address qemu loads it at
;    mov [highmove_addr],eax ; end of kernel and initrd load address
    ;mov eax, [highmove_addr] ; end of kernel and initrd load address
    ;add eax, 4096
    ;and eax, 0xfffff000
    ;mov [highmove_addr],eax ; end of kernel and initrd load address

;    mov [es:0x218], eax
;    mov edx, [initRdSize] ; ramdisk size in bytes
;    mov [es:0x21c], edx ; ramdisk size into kernel header
;    call loader



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

    jmp $

; ================= functions ====================
;length in bytes into edx
; uses flopread [hddLBA] and highmove [highmove_addr] vars
;clobbers 0x2000 segment
loader:
.loop:
    cmp edx, 127*512
    jl loader.part_2
    jz loader.finish

    mov ax, 127 ;count
    xor bx, bx ; offset
    mov cx, 0x2000 ; seg
    push edx
    call flopread
    call highmove

    mov si, pstr
    call print
    pop edx
    sub edx, 127*512

    jmp loader.loop

.part_2:   ; load less than 127*512 sectors
    shr edx, 9  ; divide by 512
    inc edx     ; increase by one to get final sector if not multiple - otherwise just load junk - doesn't matter
    mov ax, dx
    xor bx,bx
    mov cx, 0x2000
    call flopread
    call highmove

.finish:
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

err:
%ifdef DEBUG
    mov si, errStr
    call print
%endif
    jmp $

errK:
%ifdef DEBUG
    mov si, errStrK
    call print
%endif
    jmp $

err_read:
%ifdef DEBUG
    mov si, errStrRead
    call print
%endif
    jmp $

errStr db 'a20_1',0
errStrRead db 'read',0
errStrK db 'krnl',0
pstr db 'B',0


%ifdef DEBUG
; si = source str
print:
    lodsb
    and al, al
    jz print.end
    mov ah, 0xe
    mov bx, 7
    int 0x10
    jmp print
print.end:
    ret
%endif

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
    mov al, 0x01 ; read 1 sector
    mov ah, 0x02 ; 

    push si
    mov si, 20 ; set floppy retry counter
flopread.retry:
    dec si
    jz err_read
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
	times	510-($-$$)	db	0
	dw	0xaa55


; real mode print code
;    mov si, strhw
;    mov eax, 0xb8000
;    mov ch, 0x1F ; white on blue
;loop:
;    mov cl, [si]
;    mov word [ds:eax], cx
;    inc si
;    add eax, 2
;    cmp [si], byte 0
;    jnz loop

