tiny-floppy-bootloader
=====================

A fork of tiny-floppy-bootloader that is fits 2 sectors instead of 1. This bootloader expects to find the kernel immediately after it at sector 2.

## Features/Purpose

* No partition table needed 
* Easy to convert to an obfuscated loader (think anti-forensics for crypted disks)
* Easy to modify for a custom experience
* Useful in embedded devices

## Building

To build, you need to:

1. Run build.sh with the appropriate parameters:

   `./build.sh <kernel bzImage> <output image> [Floppy Size in Bytes]`

    Valid floppy sizes are: 1474560 (1.44MB), 1763328 (1.72MB), 2949120 (2.88MB)

2. Now you can dd this onto your floppy.

Your system should now boot with the new kernel.

# Troubleshooting

You can use qemu to boot the image by running:

    qemu-system-i386 -fda disk.img

and you can also connect the VM to gdb for actual debugging.  There's an included gdb script to get you started.
