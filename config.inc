%define cmdLineDef "auto"

; these set the number of cylinders and sectors assumed for the drive
; these settings are correct for your typical 1.44mb floppy
; but if you are using 1.722mb you should use 82 cylinders / 21 sectors
; for more info see https://en.wikipedia.org/wiki/List_of_floppy_disk_formats
; although they call cylinders "tracks"

; NOTE: These are now defined through the build script.
; %define nCylindersPerHeadDef 80 
; %define nSectorsPerTrackDef 18 
