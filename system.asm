[BITS 16]
[org 0x7c00]

; -- MANDATORY INITIALIZATION
cli                     ; -- pause interrupts
mov [BOOT_DRIVE], dl    ; -- move boot drive number into a symbol for later use

;; -- SET UP STACK      ; -- UNUSED because no stack routines (int, call, ret) are used before Protected Mode stack is set up
;;cli                   ; -- pause interrupts
;xor ax, ax             ; -- clear ax to 0
;mov ax, 0x7cfe         ; -- move address under beginning of code to general purpose register
;mov ss, ax             ; -- move this address to the stack segment selector
;mov sp, 0              ; -- move the stack pointer to 0 (as SS is now set to abs offset, sp should be 0)
;mov bp, sp             ; -- move the base pointer up to the stack pointer as there is nothing in the stack yet
;;sti                   ; -- re-enable interrupts

; -- ENABLE PROTECTED MODE
;cli                    ; -- disable interrupts
; -- ENABLE A20 LINE

; -- LOAD GLOBAL DESCRIPTOR TABLE
    ; -- required segment descriptors are code and data
    ; -- GDT Entry Format:
        ; -- double word 1 bitwise description
            ; -- WORD 1
            ; --  0-15: First 16 bits in the segment limit
            ; -- WORD 2
            ; -- 16-31: First 16 bits in the base address
        ; -- double word 2 bitwise description
            ; -- BYTE 1
            ; --  0- 7: Bits 16-23 in the base address
            ; -- BYTE 2
            ; --  8-12: Segment type and attributes
                ; --  8: access flag, set by CPU, leave this 0
                ; --  9: readable (code) writable (data) flag, 1 - readable/writable, 0 - not readable/writable. *code should be readable
                ; -- 10: conforming (code), if set, less privileged code can jump to or call this segment. 0 for OS
                    ; -- expand direction (data), 1 - base to limit, 0 - limit to base
                ; -- 11: specifies if code or data segment. 1 - code, 0 - data
                ; -- 12: set if either code or data segment. 1 - yes, 0 - no
            ; -- 13-14: Privilege Level, 0 - Full Control (OS) .. 3 - Least Control (User)
            ; -- 15-15: Present Flag, 1 - Present, 0 - Absent
            ; -- BYTE 3
            ; -- 16-19: Bits 16-19 in the segment limiter
            ; -- 20-22: Different attributes, depending on the segment type
                ; -- 20: Available to programmers. Put what you want
                ; -- 21: Intel Reserved. 0
                ; -- 22: Size bit. 1 - 32 bit code, 0 - 16 bit code (bit is called "Big")
            ; -- 23-23: Granularity. if 1, limit is multiplied by 4 KiB (wanted for flat segmentation)
            ; -- BYTE 4
            ; -- 24-31: The last 24-31 bits in the base address

gdt:                    ; -- begin GDT
gdt_null:               ; -- first entry is a null descriptor NEVER referenced by CPU
    dq 0                ; -- fill 2 double words ( 1 quad word ) ( a full entry ) with zero to take up space

gdt_code:               ; -- GDT entry for code segment. this should span ALL of memory. 0000 0000 - FFFF FFFF
    ; -- double word 1
    dw 0xFFFF           ; -- first half of segment limit. We want this to be FFFF FFFF eventually so FFFF makes sense
    dw 0x0000           ; -- first half of base address. We want this to be 0000 0000 eventually so 0000 makes sense
    ; -- double word 2
    db 0x00             ; -- bits 16-23 of base address.
    db 0b10011010       ; -- see above
    db 0b11001111       ; -- see above
    db 0x00             ; -- bits 24-31 of base address.

gdt_data:               ; -- GDT entry for data segment. this should also span ALL of memory. 0000 0000 - FFFF FFFF
    ; -- double word 1
    dw 0xFFFF           ; -- first half of segment limit. We want this to be FFFF FFFF eventually so FFFF makes sense
    dw 0x0000           ; -- first half of base address. We want this to be 0000 0000 eventually so 0000 makes sense
    ; -- double word 2
    db 0x00             ; -- bits 16-23 of base address.
    db 0b10010010       ; -- see above
    db 0b11001111       ; -- see above
    db 0x00             ; -- bits 24-31 of base address.
gdt_end                 ; -- symbol marking the end of the gdt in memory, for size calculation

gdt_desc:               ; -- symbol with info about out GDT
    db gdt_end - gdt    ; -- size of GDT
    dw gdt              ; -- start of GDT

xor ax, ax              ; -- clear ax register
mov ds, ax              ; -- set ds segment selector to 0 because lgdt[arg] uses ds:arg
lgdt[gdt_desc]          ; -- load global descriptor table

        ; -- can use 32 bit registers in 16 bit mode, the assembler will add the
        ; -- "Operand Size Override Prefix" (0x66) to the beginning of any instruction
mov eax, cr0            ; -- move cr0 into eax. cr0 controls, among other things, the mode of the CPU ( real - 0, protected 1)
or al, 1                ; -- set PE (Protection Enable) bit in CR0 (Control Register 0)
mov cr0, eax            ; -- put the modified cr0 value back into cr0

jmp 0x08:protected      ; -- clear pipeline of 16 bit instructions after enabling longmode by doing a long jump (jmp segment:offset)
                        ; -- GDT entries are 0x08 bits wide, so every entry is at entry_number * 0x08 : entry_point

; -- PROTECTED MODE
[BITS 32]
protected:
    xor ax, ax          ; -- clear ax register
    mov ax, 0x10        ; -- move entry offset of GDT data entry into ax
    mov ds, ax          ; -- move data segment to ax
    mov ss, ax          ; -- move stack segment to ax
    mov es, ax          ; -- move general-use segment to ax
    mov fs, ax          ; -- move general-use segment to ax
    mov gs, ax          ; -- move general-use segment to ax

;    required from protected mode before moving into long mode
;    A protected-mode IDT for vectoring interrupts and exceptions to the appropriate handlers while in protected mode.
;    The protected-mode interrupt and exception handlers referenced by the IDT.
;    Gate descriptors for each handler must be loaded in the IDT.


;    mov BYTE [0xb8000], 'P'
    ;mov al, '0'
    ;add al, BOOT_DRIVE
    ;mov [0xb8000], al
    mov BYTE [0xb8000], '0' + BOOT_DRIVE
    mov BYTE [0xb8001], 0x1b



; -- ENABLE LONG MODE
;    Disable paging
;    Set the PAE enable bit in CR4
;    Load CR3 with the physical address of the PML4
;    Enable long mode by setting the EFER.LME flag in MSR 0xC0000080
;    Enable paging

;    Now the CPU will be in compatibility mode, and instructions are still 32-bit.
;    To enter long mode, the D/B bit (bit 22, 2nd 32-bit value) of the GDT code segment must be clear
;    (as it would be for a 16-bit code segment),
;    and the L bit (bit 21, 2nd 32-bit value) of the GDT code segment must be set.
;    Once that is done, the CPU is in 64-bit long mode.

; -- LOAD KERNEL INTO MEMORY

; -- PREPARE KERNEL ENVIRONMENT

; -- HANG FOR NOW
jmp $

BOOT_DRIVE: db 0        ; -- define byte-sized symbol to store boot drive number

times  510-($-$$) db 0  ; -- pad zeros up to byte 510
dw 0xaa55               ; -- write magic boot bytes at bytes 511 and 512

; -- END OF LOADED SECTOR
