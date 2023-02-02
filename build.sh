#!/bin/bash -e
# Tiny Linux Bootloader
# (c) 2014- Dr Gareth Owen (www.ghowen.me). All rights reserved.
# Modifications by Eric Voirin (oerg866@googlemail.com).

#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

INPUT="bflop.asm"
OUTPUT="$2"
KERN="$1"
DISKSIZE="1474560" # Default = 1.44M floppy
CYLINDERS=0
SECTORS=0

if [[ "$#" -lt 2 ]]; then
    echo "Insufficient parameters!"
    echo "Syntax: ./build.sh <kernel bzImage> <output image> [Floppy Size in Bytes]"
    echo "Size parameter is optional and can be one of these:"
    echo "  1474560"
    echo "  1763328"
    echo "  2949120"
    exit 127
fi

if [[ "$#" -gt 2 ]]; then
    DISKSIZE="$3"
fi

# Get Cylinders per head for this size, we have a few hardcoded ones.
if   [[ "$DISKSIZE" -eq 1474560 ]]; then
    CYLINDERS=80
    SECTORS=18
elif [[ "$DISKSIZE" -eq 1763328 ]]; then
    CYLINDERS=82
    SECTORS=21
elif [[ "$DISKSIZE" -eq 2949120 ]]; then
    CYLINDERS=80
    SECTORS=36
else
    echo "Unknown disk size specified!!! Aborting!"
    exit 127
fi

#size of kernel
K_SZ=`stat -c %s $KERN`

# Padding for the boot loader, 2 sectors á 512 bytes -> 1024 bytes
K_PAD=$((1024 - $K_SZ % 1024))

nasm -D nCylindersPerHeadDef=$CYLINDERS -D nSectorsPerTrackDef=$SECTORS -o $OUTPUT $INPUT
cp $OUTPUT bootloader.bin

cat $KERN >> $OUTPUT
if [[ $K_PAD -lt 1024 ]]; then
    dd if=/dev/zero bs=1 count=$K_PAD >> $OUTPUT
fi

# make an objdump of the bootloader for debugging purposes
objdump -b binary --adjust-vma=0x7c00 -D bootloader.bin -m i8086 -M intel > objdump_out.objdump

TOTAL=`stat -c %s $OUTPUT`
if [[ $TOTAL -gt $DISKSIZE ]]; then
    echo "Warning: Floppy image exceeds requrested size $DISKSIZE!!!"
else
    dd if=/dev/zero bs=1 count=$(($DISKSIZE - $TOTAL)) >> $OUTPUT
fi

echo "concatenated bootloader, kernel and initrd into ::> $OUTPUT"
