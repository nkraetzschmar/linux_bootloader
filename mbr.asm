org 0x7c00
BITS 16

; memory layout:
;
; === low mem ===
;
;        ? - 0x007c00    stack (grows down dynamically)
; 0x007c00 - 0x007dbe    bootloader
; 0x007dbe - 0x007dfe    partition table
; 0x007dfe - 0x007e00    boot signature
;
; 0x010000 - 0x018000    real mode kernel code
; 0x018000 - 0x01f000    real mode kernel heap
; 0x01f000 - 0x01f800    kernel cmdline
; 0x040000 - 0x048000    disk read temp buffer (for loading protected mode kernel code)
;
; === high mem ===
;
; 0x100000 - ?           protected mode kernel code

kernel_sectors: equ 0x66ca                                      ; total number of kernel sectors
kernel_lba:     equ 0x00000021                                  ; LBA location of the kernel on disk

real_kernel_sectors: equ 0x0028                                 ; size of the real mode kernel code in sectors
real_kernel_addr:    equ 0x010000                               ; memory address of the real mode kernel code
real_kernel_seg:     equ real_kernel_addr / 0x10                ; memory segment of the real mode kernel code

prot_kernel_lba:      equ kernel_lba + real_kernel_sectors      ; LBA location of the protected mode kernel code on disk
prot_kernel_sectors:  equ kernel_sectors - real_kernel_sectors  ; size of the protected mode kernel code in sectors
prot_kernel_addr:     equ 0x100000                              ; memory address of the protected mode kernel code
prot_kernel_base_low: equ prot_kernel_addr % 0x010000           ; low 16 bits of protected mode kernel code address
prot_kernel_base_mid: equ prot_kernel_addr / 0x010000           ; mid 8 bits of protected mode kernel code address

buffer_addr:     equ 0x040000                                   ; memory address of disk read buffer
buffer_base_low: equ buffer_addr % 0x010000                     ; low 16 bits of buffer address
buffer_base_mid: equ buffer_addr / 0x010000                     ; mid 8 bits of buffer address
buffer_seg:      equ buffer_addr / 0x10                         ; memory segment of disk read buffer

cmdline:      equ 0x01f000                                      ; memory oddress of the cmdline
kernel_stack: equ cmdline - real_kernel_addr                    ; sp for kernel entry
heap_end_ptr: equ kernel_stack - 0x0200                         ; offset from the real mode kernel code to the end of heap minus 0x0200 (as according to linux boot protocol spec)

main:
; perform basic init functions and setup stack
	cli                         ; disable interrupts
	mov ax, 0x0000              ; set ax = 0
	mov ds, ax                  ; ensure data segment at 0
	mov ss, ax                  ; ensure stack segment at 0
	mov es, ax                  ; ensure extra segment at 0
	mov sp, 0x7c00              ; set stack pointer to start of bootloader code (grows down below it)
	cld                         ; ensure forward direction for string operations (lodsb)

; read real mode kernel code
	mov si, dap                 ; point si at dap (pre-initialized with source and destination addresses for real mode kernel code load)
	mov dl, 0x80                ; select disk 0x80 (primary HDD) for disk access
	mov ah, 0x42                ; select LBA read mode for disk access
	int 0x13                    ; perform disk access via interrupt
	jc  disk_read_error         ; on error jump to print errror msg and halt

; read protected mode kernel code

	mov word  [dap.sectors], 0x0040             ; set dap to read 64 sectors (32 KiB)
	mov word  [dap.segment], buffer_seg         ; set dap to tarteg the buffer segment
	mov dword [dap.lba],     prot_kernel_lba    ; set dap to read from protected mode kernel code LBA

	mov cx, prot_kernel_sectors ; set cx to number of protected mode kernel sectors
	shr cx, 0x06                ; divide by 64 (the number of sectors to read in one chunk)

.loop:
	cmp cx, 0x0000              ; check if we have read all full chunks
	je  .end                    ; if then exit loop

	mov si, dap                 ; point si at dap
	mov dl, 0x80                ; select disk 0x80 (primary HDD) for disk access
	mov ah, 0x42                ; select LBA read mode for disk access
	int 0x13                    ; perform disk access via interrupt (all registers still set from previous call)
	jc  disk_read_error         ; on error jump to print errror msg and halt

	mov dx, cx                  ; store cx in dx
	mov cx, 0x4000              ; set size of data to copy to 16K words = 32KiB
	mov si, gdt                 ; point es:gdt at gdt
	mov ah, 0x87                ; select block mov mode
	int 0x15                    ; perform block mov via interrupt
	jc  block_move_error        ; on error jump to print error msg and holt
	mov cx, dx                  ; restore saved cx

	add word [dap.lba],             0x0040      ; advance dap LBA by 64 sectors
	add word [gdt.target_base_low], 0x8000      ; advance target base by 32 KiB
	adc byte [gdt.target_base_mid], 0x00        ; if target base low rolled over then advance target base mid

	dec cx                      ; decrement number of chunks left to read
	jmp .loop                   ; continue loop

.end:

	mov ax, prot_kernel_sectors ; set cx to number of protected mode kernel sectors
	and ax, 0x63                ; compute mod 64

	cmp ax, 0x0000              ; check for no remainder
	je  .skip                   ; skip if no remaining sectors

	mov [dap.sectors], ax       ; set dap to read remaining sectors
	mov cx, ax                  ; copy remaining sectors to cx

	mov si, dap                 ; point si at dap
	mov dl, 0x80                ; select disk 0x80 (primary HDD) for disk access
	mov ah, 0x42                ; select LBA read mode for disk access
	int 0x13                    ; perform disk access via interrupt (all registers still set from previous call)
	jc  disk_read_error         ; on error jump to print errror msg and halt

	shl cx, 8                   ; multiply number of final sectors by 256 to get number of words
	mov si, gdt                 ; point es:gdt at gdt
	mov ah, 0x87                ; select block mov mode
	int 0x15                    ; perform block mov via interrupt
	jc  block_move_error        ; on error jump to print error msg and holt

.skip:

; set kernel header fields
	mov ax, real_kernel_seg     ; set ax = segment of real mode kernel code
	mov ds, ax                  ; set extra segment to real mode kernel code location

	mov    byte  [0x210], 0xff                ; set type_of_loader = undefined
	or     byte  [0x211], 0x80                ; set CAN_USE_HEAP bit in loadflags
	mov    word  [0x224], heap_end_ptr        ; set heap_end_ptr
	mov    dword [0x228], cmdline             ; set cmdline

; print kernel version
	mov si, [0x020e]            ; load kernel_version ptr from header
	add si, 0x0200              ; add 0x0200 offset (somehow needed according to spec)
	call print_str              ; print the kernel version string
	mov al, 0x0d                ; load \r into al
	call print_char             ; print \r
	mov al, 0x0a                ; load \n into al
	call print_char             ; print \n
	mov al, ' '                 ; load space into al
	call print_char             ; print space char to ensure character position is advanced
	mov al, 0x0d                ; load \r into al
	call print_char             ; print \r

; execute the kernel
.exec:
	mov ax, ds                  ; copy the data segment into ax and from there into all other segment registers
	mov es, ax                  ; extra segment
	mov fs, ax                  ; fs segment
	mov gs, ax                  ; gs segment
	mov ss, ax                  ; stack segment
	mov sp, kernel_stack        ; set the stack pointer to top of kernel heap
	jmp 0x1020:0                ; far jump to kernel entry point

halt:
	hlt
	jmp halt

; prints a single character
; inputs:
;   al: the character to print
; clobbers: ax, bx
print_char:
	mov ah, 0x0e                ; select teletype output mode
	mov bh, 0x00                ; ensure page number is 0
	int 0x10                    ; perform write via interrupt
	ret

; prints a null terminated string
; inputs:
;   ds:si: points to start of string
; clobbers: ax, bx, si
print_str:
	lodsb                       ; load byte pointed to by ds:si into al and increment si
	cmp   al, 0x00              ; check if al == 0, i.e. we read a null byte
	je    .done                 ; if al is null byte jump to .done
	call  print_char            ; else print the byte in al
	jmp   print_str             ; loop until null byte was read
.done:
	ret                         ; return from call

; print disk read error and halts
disk_read_error:
	mov  ax, 0x0000             ; set ax = 0
	mov  ds, ax                 ; reset data segment to 0
	mov  si, .msg               ; load .msg addr into si
	call print_str              ; print .msg
	jmp  halt                   ; halt

.msg: db "disk read error", 0x0d, 0x0a 0

; print block move error and halts
block_move_error:
	mov  dl, ah
	mov  ax, 0x0000             ; set ax = 0
	mov  ds, ax                 ; reset data segment to 0
	mov  si, .msg               ; load .msg addr into si
	call print_str              ; print .msg
	jmp  halt                   ; halt

.msg: db "block move error", 0x0d, 0x0a 0

; disk address packet
dap:
	db 0x10                     ; size of struct = 16
	db 0x00                     ; unused
.sectors:
	dw real_kernel_sectors      ; number of sectors to read
	dw 0x0000                   ; memory offset within segment (always 0 here)
.segment:
	dw real_kernel_seg          ; memory segment
.lba:
	dd kernel_lba               ; low bytes of LBA address of data on disk
	dd 0x00000000               ; high bytes of LBA address (unused by us)

; global descriptor table
gdt:
	times 16 db 0               ; obligatory null entries
; source segment (0x040000 - 0x048000)
	dw 0x7fff                   ; limit low bits (32KiB)
	dw buffer_base_low          ; base low bits
	db buffer_base_mid          ; base mid bits
	db 0x93                     ; access mode (present, type=data, rw)
	db 0x00                     ; flags and limit high bits
	db 0x00                     ; base high bits
; target segment in high mem, initially at 0x100000 - 0x108000, updated as a sliding window
	dw 0x7fff                   ; limit low bits (32KiB)
.target_base_low:               ; label to allow updating base
	dw prot_kernel_base_low     ; base low bits
.target_base_mid:               ; label to allow updating base
	db prot_kernel_base_mid     ; base mid bits
	db 0x93                     ; access mode (present, type=data, rw)
	db 0x00                     ; flags and limit high bits
	db 0x00                     ; base high bits
	times 16 db 0               ; obligatory null entries

; assert that we have not over-run the maximum size of an MBR bootloader
%if ($-$$) > 446
	%error "MBR code exceeds 446 bytes"
%endif

times 510-($-$$) db 0           ; padd the remaining space with zeros
dw 0xaa55                       ; add the required boot signature
