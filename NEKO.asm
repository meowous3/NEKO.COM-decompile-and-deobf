; =============================================================================
; NEKO.COM - Annotated Disassembly
; =============================================================================
;
; A TSR (Terminate and Stay Resident) program for the NEC PC-9801 that
; displays an animated cat sprite that chases the text cursor (or mouse
; cursor) across the graphics VRAM.
;
; Original authors: <tenten> & naoshi
; Version: 0.70
;
; Platform:  NEC PC-9801 (MS-DOS, real mode, 16-bit x86)
; Format:    DOS .COM file (loads at CS:0100h)
;
; How it works:
;   1. On first run, hooks the VSYNC interrupt (INT 0Ah / IRQ2) to get
;      called ~56 times per second (or ~31 in hi-res mode).
;   2. Each VSYNC tick decrements a counter; when it reaches zero, one
;      animation step is performed:
;        a) Read the cursor/mouse position
;        b) Decide which direction the cat should move (or sit/scratch)
;        c) Erase the old cat sprite from VRAM
;        d) Draw the new cat sprite to VRAM
;        e) Advance the animation sub-frame
;   3. The cat has 7 animation states (up, down, right, left, sit-right,
;      sit-left, scratch) each with 4 frames, stored as 32x32 1bpp
;      bitmaps across 3 color planes.
;   4. When the cat reaches the cursor it sits down, and after an idle
;      period it may scratch or wander to a random location.
;
; Command-line flags:
;   -r  Remove the resident copy from memory
;   -g  Show the GDC hardware text cursor (normally hidden)
;   -m  Use INT 33h mouse driver for cursor position
;   -o  Offset mode: cat stops 4 columns away from cursor
;   -f  Add another cat (increases speed for each additional cat)
;
; PC-9801 hardware reference:
;   VRAM planes:   0A800h (blue), 0B000h (red), 0B800h (green)
;   VRAM layout:   640x400 pixels, 80 bytes per scanline, linear
;   VSYNC status:  port 0A0h (bit 2 = in blanking, bit 3 = not blanking)
;   PIC mask:      port 02h (bit 2 = IRQ2 / VSYNC)
;   PPI ports:     60h (status), 62h (data) -- used for cursor position
;   GDC display:   port 0A6h (display page select)
;   INT 0Ah:       VSYNC interrupt (IRQ2 on master PIC)
;   INT 18h:       PC-98 keyboard BIOS
;   INT 1Bh:       PC-98 break / Ctrl+C handler
;   INT 21h:       DOS services
;   INT 33h:       Mouse driver (optional)
;   EOI port:      00h (write 20h to acknowledge interrupt)
;   Misc I/O:      64h (interrupt acknowledge / edge trigger clear)
;
; Sprite format:
;   Each frame is 32 pixels wide x 32 pixels tall.
;   Stored as 3 consecutive plane bitmaps (blue, red, green).
;   Each plane: 32 rows x 4 bytes = 128 bytes.
;   Total per frame: 128 x 3 = 384 (0x180) bytes.
;   7 states x 4 frames = 28 frames total = 10752 (0x2A00) bytes.
;
; Notes on NEKO_FIXED.COM:
;   This binary appears to be a post-compilation patched version of the
;   original NEKO.COM. The patching introduced several byte-offset errors:
;     1. The entry JMP at 0x0100 targets 0x3303 instead of the intended
;        0x32FB (or 0x3302), landing mid-instruction.
;     2. String references in the setup code are off by 2-5 bytes,
;        truncating the "Neko" prefix from most messages.
;   These are documented in detail at the relevant code locations.
;   The annotated disassembly below presents the INTENDED code logic
;   as reconstructed from the binary, with bug notes where applicable.
;
; Memory map (after loading at CS:0100h):
;   0100h-0102h   Entry jump
;   0103h-010Fh   Metadata (magic number, version string)
;   0110h-0252h   Runtime variables (BSS-like, mostly zero on disk)
;   0253h-0595h   Resident code (ISRs and subroutines)
;   0596h-0597h   PRNG seed (2 bytes)
;   0598h-0717h   Blank sprite (384 zero bytes, used for erasing)
;   0718h-3117h   Sprite data (7 states x 4 frames x 384 bytes)
;   3118h-3136h   PRNG function (called via far pointer)
;   3137h-3215h   DOS strings (dollar-terminated)
;   3216h-3471h   Setup / transient code (freed after TSR install)
;
; =============================================================================

        cpu     8086
        org     0x0100

; =============================================================================
; SECTION 1: ENTRY POINT AND HEADER
; =============================================================================

entry:
        jmp     0x3303                  ; 0100: JMP to setup code
                                        ;
                                        ; BUG IN NEKO_FIXED.COM:
                                        ; The encoded displacement (E9 00 32) targets
                                        ; 0x3303, which lands 1 byte into the 4-byte
                                        ; instruction "mov [data_seg_val], ds" at 0x3302.
                                        ; The intended target was likely 0x32FB (setup_banner)
                                        ; to print the version banner and then fall through
                                        ; to the initialization code at 0x3302.
                                        ;
                                        ; This annotated disassembly presents the INTENDED
                                        ; code flow starting from setup_banner (0x32FB),
                                        ; which prints the banner and falls through to
                                        ; setup_entry (0x3302) for variable initialization.

; -----------------------------------------------------------------------------
; Metadata block (0x0103 - 0x010F)
; This region is also used during the "already resident?" check:
; the setup code compares its own resident code region with the
; copy found at the INT 0Ah vector's segment to detect a prior
; installation.
; -----------------------------------------------------------------------------
metadata:
        db      0x00, 0x00              ; 0103: reserved / padding
        db      0x00, 0x00, 0x00        ; 0105: reserved
        dw      0x3412                  ; 0108: magic signature
        db      0x18, 0x07              ; 010A: date or build info
version_str:
        db      "0.70"                  ; 010C: version string (4 bytes)

; =============================================================================
; SECTION 2: RUNTIME VARIABLES
; =============================================================================
; These are the BSS-like variables used by the resident code at runtime.
; Most are zero-initialized on disk; they are filled in during setup or
; modified at runtime by the VSYNC ISR.
; -----------------------------------------------------------------------------

        db      0x00                    ; 0110: padding
        db      0x90                    ; 0111: NOP opcode (padding/alignment)

cat_x:
        dw      0x0000                  ; 0112: cat X position (byte column, 0-76)
                                        ;   Each unit = 8 pixels horizontally
cat_y:
        dw      0x0000                  ; 0114: cat Y position (8-pixel row, 0-46)
                                        ;   Each unit = 8 scanlines vertically
anim_state:
        dw      0x0000                  ; 0116: animation state
                                        ;   0 = moving up
                                        ;   1 = moving down
                                        ;   2 = moving right
                                        ;   3 = moving left
                                        ;   4 = sitting (facing right)
                                        ;   5 = sitting (facing left)
                                        ;   6 = scratching
idle_counter:
        dw      0x0000                  ; 0118: idle tick counter (counts up while sitting)
saved_vram_off:
        dw      0xFFFF                  ; 011A: VRAM offset where cat was last drawn
                                        ;   0xFFFF = "nothing drawn yet"
saved_int0a_off:
        dw      0x0000                  ; 011C: saved INT 0Ah (VSYNC) vector - offset
saved_int0a_seg:
        dw      0x0000                  ; 011E: saved INT 0Ah (VSYNC) vector - segment
saved_int18_off:
        dw      0x0000                  ; 0120: saved INT 18h (keyboard BIOS) vector - offset
saved_int18_seg:
        dw      0x0000                  ; 0122: saved INT 18h (keyboard BIOS) vector - segment
saved_int1b_off:
        dw      0x0000                  ; 0124: saved INT 1Bh (break) vector - offset
saved_int1b_seg:
        dw      0x0000                  ; 0126: saved INT 1Bh (break) vector - segment
        dw      0x0000                  ; 0128: (unused/reserved)
raw_cursor_pos:
        dw      0x0000                  ; 012A: raw cursor position (linear, 0-1999)
cursor_x:
        dw      0x0000                  ; 012C: cursor X (column 0-79)
cursor_y:
        dw      0x0000                  ; 012E: cursor Y (text row 0-24)
anim_frame:
        dw      0x0000                  ; 0130: animation sub-frame (cycles 0, 1, 2, 3)
tick_counter:
        dw      0x000A                  ; 0132: VSYNC tick countdown to next update
                                        ;   Reloaded from tick_rate after each update

; --- Gap: 0x0134 to 0x0233 ---
; This region is all zeros on disk. At runtime, the area from about 0x0153
; downward to 0x0134 serves as the ISR's private stack (grows downward
; from saved_sp at 0x0234).
        times (0x0234 - 0x0134) db 0x00

; --- Upper variable region (0x0234 - 0x0252) ---
; These variables are at higher addresses due to the stack layout:
; the ISR switches SP to point at saved_sp (0x0234), so the stack
; grows downward into the zero-filled gap above.

saved_sp:
        dw      0x0000                  ; 0234: saved SP during ISR stack switch
saved_ss:
        dw      0x0000                  ; 0236: saved SS during ISR stack switch
rng_ptr_off:
        dw      0x0000                  ; 0238: far pointer to PRNG function - offset
rng_ptr_seg:
        dw      0x0000                  ; 023A: far pointer to PRNG function - segment
idle_threshold:
        dw      0x0064                  ; 023C: idle ticks before cat gets bored
                                        ;   Default 100; increased by 10 for each -f cat
tick_rate:
        dw      0x000A                  ; 023E: VSYNC ticks between animation updates
                                        ;   Default 10 (~5.6 FPS at 56Hz VSYNC)
                                        ;   Set to 2 in hi-res mode (~28 FPS at 31Hz)
                                        ;   Increased by 1 for each -f cat
cat_speed:
        dw      0x0000                  ; 0240: horizontal stopping offset
                                        ;   0 = cat sits on cursor position
                                        ;   4 = cat stops 4 byte-columns from cursor (-o flag)
                                        ;   Increases by 4 for each -f cat
data_seg_val:
        dw      0x0000                  ; 0242: DS segment value (set during init)
                                        ;   Used by draw_sprite to access sprite data
wander_target_x:
        dw      0x0000                  ; 0244: random wander target X column (0-79)
wander_target_y:
        dw      0x0000                  ; 0246: random wander target Y row (0-23)
saved_bp_1b:
        dw      0x0000                  ; 0248: saved BP for INT 1Bh wrapper
saved_sp_1b:
        dw      0x0000                  ; 024A: saved SP for INT 1Bh wrapper
flag_show_cursor:
        db      0x00                    ; 024C: show GDC text cursor (1 if -g flag)
flag_mouse_drv:
        db      0x00                    ; 024D: use INT 33h mouse driver (1 if -m flag)
reentrance_guard:
        db      0x00                    ; 024E: reentrance lock for VSYNC ISR
                                        ;   0 = safe to enter, 1 = already in ISR
flag_wander:
        db      0x00                    ; 024F: random wander mode active
                                        ;   0 = chasing cursor, 1 = walking to random spot
flag_dir_changed:
        db      0x00                    ; 0250: direction-changed flag
                                        ;   Set to 1 when cat starts scratching,
                                        ;   prevents immediate re-trigger of scratch
saved_pic_mask:
        db      0x00                    ; 0251: saved PIC mask byte for INT 1Bh handler
flag_hires:
        db      0x00                    ; 0252: high-resolution mode detected
                                        ;   If 1, VSYNC rate is ~31Hz instead of ~56Hz

; =============================================================================
; SECTION 3: RESIDENT CODE (stays in memory after TSR)
; =============================================================================
; This section contains all the subroutines that run during the VSYNC
; interrupt. Everything from here (0x0253) through 0x0595 remains
; resident in memory after the TSR call.
; =============================================================================

; -----------------------------------------------------------------------------
; wait_vsync: Wait for the next vertical blanking interval
; -----------------------------------------------------------------------------
; Synchronizes with the display's vertical retrace to prevent visible
; tearing when drawing sprites. On the PC-9801, port 0A0h provides
; the VSYNC status:
;   Bit 2 (0x04): Set during vertical blanking period
;   Bit 3 (0x08): Set during active display (not blanking)
;
; The routine first waits for blanking to START (bit 2 becomes set),
; then waits for the active period to resume (bit 3 becomes clear),
; ensuring we are at the very beginning of a new frame.
;
; The three JMP SHORT $+2 instructions between the two polling loops
; are deliberate I/O delays, giving the CRT controller time to
; transition states.
; -----------------------------------------------------------------------------
wait_vsync:                             ; 0253
        cli                             ; disable interrupts during sync
.wait_blank_start:                      ; 0254
        in      al, 0xA0               ; read VSYNC status register
        test    al, 0x04               ; bit 2: vertical blanking active?
        jz      .wait_blank_start      ; not yet -> keep polling
        jmp     short $+2              ; 025A: I/O delay (short jump to next insn)
        jmp     short $+2              ; 025C: I/O delay
        jmp     short $+2              ; 025E: I/O delay
.wait_blank_end:                        ; 0260
        in      al, 0xA0               ; read VSYNC status again
        test    al, 0x08               ; bit 3: active display period?
        jnz     .wait_blank_end        ; still blanking -> keep polling
        sti                             ; 0266: re-enable interrupts
        ret                             ; 0267

; -----------------------------------------------------------------------------
; calc_vram_offset: Calculate VRAM byte offset for current cat position
; -----------------------------------------------------------------------------
; Computes the linear byte offset into a VRAM plane for the cat's
; current (X, Y) position.
;
; Input:   [cat_x] = byte column (0-76)
;          [cat_y] = 8-scanline row (0-46)
; Output:  DI = byte offset into VRAM plane
; Clobbers: AX, DX (saved/restored via stack), CX popped
;
; Formula: DI = clamp(cat_y, 0..46) * 640 + cat_x
;
; Why 640? Each row unit is 8 scanlines, and each scanline is 80 bytes:
;   8 * 80 = 640 bytes per row unit.
;
; The clamping ensures the 32-pixel-tall sprite stays on screen:
;   Max row 46: 46*8 + 32 = 400 scanlines = full screen height.
;   At column 0-76: sprite width 4 bytes, so max column = 76 (76+4=80).
; -----------------------------------------------------------------------------
calc_vram_offset:                       ; 0268
        push    ax                      ; save AX
        push    dx                      ; save DX
        push    cx                      ; save CX
        mov     ax, [cat_y]             ; AX = cat Y position (8-scanline rows)
        cmp     ax, 0x002E              ; is Y > 46?
        jl      .y_not_over             ; no -> skip
        mov     ax, 0x002E              ; yes -> clamp to 46
.y_not_over:                            ; 0276
        cmp     ax, 0x0000              ; is Y < 0?
        jnl     .y_not_under            ; no -> skip
        mov     ax, 0x0000              ; yes -> clamp to 0
.y_not_under:                           ; 027E
        mov     dx, 0x0280              ; DX = 640 (80 bytes/line * 8 lines/row)
        mul     dx                      ; AX = cat_y * 640
        add     ax, [cat_x]            ; AX += cat_x (byte column offset)
        mov     di, ax                  ; DI = final VRAM byte offset
        pop     cx                      ; restore CX
        pop     dx                      ; restore DX
        pop     ax                      ; restore AX
        ret                             ; 028C

; -----------------------------------------------------------------------------
; blit_sprite_plane: Copy one plane of a sprite to a VRAM plane
; -----------------------------------------------------------------------------
; Copies 32 rows of 4 bytes each from the sprite data buffer (DS:SI)
; into a VRAM plane (ES:DI). After each row of 4 bytes, the VRAM
; pointer advances by 80 bytes total (4 copied + 76 skipped) to
; reach the next scanline.
;
; Input:   AX = VRAM plane segment (0A800h, 0B000h, or 0B800h)
;          DS:SI = pointer to 128 bytes of sprite plane data
;          DI = VRAM byte offset within the plane
; Output:  SI advanced by 128 bytes (ready for next plane)
;          DI restored to original value
; Clobbers: BX, CX (saved/restored)
; -----------------------------------------------------------------------------
blit_sprite_plane:                      ; 028D
        push    es                      ; save ES
        push    bx                      ; save BX
        push    cx                      ; save CX
        push    di                      ; save VRAM offset (restored after blit)
        mov     es, ax                  ; ES = target VRAM plane segment
        mov     bx, 0x0020              ; BX = 32 rows to copy
.row_loop:                              ; 0296
        mov     cx, 0x0002              ; CX = 2 words (4 bytes) per row
        rep     movsw                   ; copy 4 bytes from DS:SI to ES:DI
                                        ;   SI += 4, DI += 4
        add     di, 0x004C              ; DI += 76 -> advance to next scanline
                                        ;   (total stride: 4 + 76 = 80 bytes)
        dec     bx                      ; one less row to do
        jnz     .row_loop              ; loop until all 32 rows done
        pop     di                      ; restore DI to start position
        pop     cx                      ; restore CX
        pop     bx                      ; restore BX
        pop     es                      ; restore ES
        ret                             ; 02A5

; -----------------------------------------------------------------------------
; draw_sprite: Draw a 32x32 sprite at the current cat position
; -----------------------------------------------------------------------------
; Draws a full sprite (3 color planes) at the VRAM location
; corresponding to the current cat_x and cat_y coordinates.
;
; Input:   SI = pointer to sprite data (384 bytes: blue + red + green)
; Output:  SI advanced by 384 bytes
;
; The function computes the VRAM offset, sets DS to the code segment
; (so SI can address sprite data within our memory), and then blits
; each of the three VRAM planes in sequence.
; -----------------------------------------------------------------------------
draw_sprite:                            ; 02A6
        push    ax                      ; save AX
        push    ds                      ; save DS
        call    calc_vram_offset        ; DI = VRAM offset for (cat_x, cat_y)
        mov     ax, [data_seg_val]      ; AX = our code/data segment
        mov     ds, ax                  ; DS = our segment (sprite data is in CS)
        mov     ax, 0xA800             ; blue VRAM plane
        call    blit_sprite_plane       ; blit 128 bytes (blue plane)
        mov     ax, 0xB000             ; red VRAM plane
        call    blit_sprite_plane       ; blit 128 bytes (red plane)
        mov     ax, 0xB800             ; green VRAM plane
        call    blit_sprite_plane       ; blit 128 bytes (green plane)
        pop     ds                      ; restore DS
        pop     ax                      ; restore AX
        ret                             ; 02C4

; -----------------------------------------------------------------------------
; draw_cat: Full draw cycle - hide mouse, erase old, draw new, show mouse
; -----------------------------------------------------------------------------
; The main rendering entry point called once per animation update.
; Coordinates the complete sequence of:
;   1. Hiding the INT 33h mouse cursor (if -m flag active)
;   2. Erasing the cat from its previous VRAM position
;   3. Computing the VRAM offset for the new position
;   4. Drawing the cat sprite at the new position
;   5. Showing the INT 33h mouse cursor again
;
; The mouse hide/show prevents flicker when the cat overlaps the
; mouse cursor area.
; -----------------------------------------------------------------------------
draw_cat:                               ; 02C5
        push    di                      ; save DI
        cmp     byte [flag_mouse_drv], 0x00  ; is INT 33h mouse driver active?
        jz      .no_hide_mouse          ; no -> skip mouse hide
        mov     ax, 0x0002              ; INT 33h function 2: hide mouse cursor
        int     0x33                    ; hide mouse
.no_hide_mouse:                         ; 02D2
        call    erase_old_sprite        ; erase cat from previous position
        call    calc_vram_offset        ; DI = new VRAM offset for current cat pos
        mov     [saved_vram_off], di    ; save offset for next erase cycle
        call    draw_sprite             ; draw cat sprite at new position
        cmp     byte [flag_mouse_drv], 0x00  ; mouse driver active?
        jz      .no_show_mouse          ; no -> skip
        mov     ax, 0x0001              ; INT 33h function 1: show mouse cursor
        int     0x33                    ; show mouse
.no_show_mouse:                         ; 02EB
        pop     di                      ; restore DI
        ret                             ; 02EC

; -----------------------------------------------------------------------------
; erase_old_sprite: Erase the cat from its previous VRAM position
; -----------------------------------------------------------------------------
; Checks saved_vram_off: if it is 0xFFFF (sentinel value meaning
; "nothing has been drawn yet"), the function returns immediately.
; Otherwise, it draws the blank_sprite (384 bytes of zeros) at the
; saved VRAM offset, effectively clearing the cat image, then resets
; the saved offset to 0xFFFF.
; -----------------------------------------------------------------------------
erase_old_sprite:                       ; 02ED
        push    di                      ; save DI
        mov     di, [saved_vram_off]    ; DI = offset where cat was last drawn
        cmp     di, 0xFFFF             ; sentinel: was anything drawn?
        jz      .nothing_to_erase      ; 0xFFFF -> nothing to erase
        push    si                      ; save SI
        mov     si, blank_sprite        ; SI = address of 384 zero bytes (0x0598)
        call    draw_sprite             ; overwrite old cat position with zeros
        mov     word [saved_vram_off], 0xFFFF  ; mark as "nothing drawn"
        pop     si                      ; restore SI
.nothing_to_erase:                      ; 0305
        pop     di                      ; restore DI
        ret                             ; 0306

; -----------------------------------------------------------------------------
; lookup_sprite: Get pointer to current animation frame's sprite data
; -----------------------------------------------------------------------------
; Calculates the address of the 384-byte sprite data block for the
; current combination of animation state and sub-frame.
;
; Output:  SI = pointer to sprite data (384 bytes)
;
; Layout in memory:
;   sprite_data + (anim_state * 1536) + (anim_frame * 384)
;
; Where:
;   1536 = 4 frames/state * 384 bytes/frame = 0x0600
;   384  = 3 planes * 128 bytes/plane       = 0x0180
; -----------------------------------------------------------------------------
lookup_sprite:                          ; 0307
        mov     si, sprite_data         ; SI = base of sprite data (0x0718)
        mov     ax, 0x0600              ; AX = 1536 bytes per state (4 frames)
        mul     word [anim_state]       ; AX = state_index * 1536
        add     si, ax                  ; SI += state offset
        mov     ax, 0x0180              ; AX = 384 bytes per frame
        mul     word [anim_frame]       ; AX = frame_index * 384
        add     si, ax                  ; SI += frame offset
        ret                             ; 031C: SI now points to correct sprite

; -----------------------------------------------------------------------------
; io_delay: Tiny delay for I/O port timing
; -----------------------------------------------------------------------------
; A near RET used as a multi-cycle delay between consecutive I/O port
; accesses. The CALL + RET overhead provides enough cycles for the
; hardware to settle. This is a common technique on 8086/V30 systems.
; -----------------------------------------------------------------------------
io_delay:                               ; 031D
        ret

; -----------------------------------------------------------------------------
; read_cursor_pos: Read the text cursor position via 8255 PPI
; -----------------------------------------------------------------------------
; On the PC-9801, the text cursor position can be read through the
; system's 8255 PPI (Programmable Peripheral Interface):
;
;   Port 60h = PPI status register
;     Bit 2: Controller ready (can accept command)
;     Bit 0: Data ready (response available)
;   Port 62h = PPI data/command register
;     Write: send command byte
;     Read: receive response byte
;
; Protocol:
;   1. CLI to prevent interference during the PPI transaction
;   2. Poll port 60h until bit 2 is set (controller ready)
;   3. Write command 0xE0 to port 62h (request cursor position)
;   4. Poll port 60h until bit 0 is set (data ready)
;   5. STI (safe to re-enable interrupts now)
;   6. Read 5 bytes from port 62h:
;        Byte 1 -> DL (position low byte)
;        Byte 2 -> DH (position high byte)
;        Bytes 3-5 are read and discarded
;   7. DX = raw linear cursor position
;
; The raw position is:
;   - Clamped to range [0, 1999] (0x0000 to 0x07CF)
;   - Divided by 80 to get:
;       Quotient  = row    (0-24)  -> cursor_y
;       Remainder = column (0-79)  -> cursor_x
;
; Output: [raw_cursor_pos], [cursor_x], [cursor_y] updated
; Clobbers: AX, BX, DX (saved/restored via stack)
; -----------------------------------------------------------------------------
read_cursor_pos:                        ; 031E
        push    ax                      ; save registers
        push    bx
        push    dx
        cli                             ; disable interrupts for PPI access

.wait_ppi_ready:                        ; 0322
        in      al, 0x60               ; read PPI status register
        call    io_delay               ; small delay for I/O timing
        test    al, 0x04               ; bit 2: controller ready?
        jz      .wait_ppi_ready        ; not ready -> poll again

        mov     al, 0xE0               ; command: request cursor position
        out     0x62, al               ; send command to PPI data register
        call    io_delay               ; I/O timing delay

.wait_data_ready:                       ; 0332
        in      al, 0x60               ; read PPI status
        call    io_delay               ; I/O timing delay
        test    al, 0x01               ; bit 0: response data available?
        jz      .wait_data_ready       ; not ready -> poll again

        sti                             ; 033B: re-enable interrupts (data is ready)

        ; Read the 5-byte response from the PPI
        in      al, 0x62               ; byte 1: position low byte
        call    io_delay
        mov     dl, al                  ; DL = low byte of position

        in      al, 0x62               ; byte 2: position high byte
        call    io_delay
        mov     dh, al                  ; DH = high byte -> DX = raw position

        in      al, 0x62               ; byte 3: (additional data, discarded)
        call    io_delay
        in      al, 0x62               ; byte 4: (discarded)
        call    io_delay
        in      al, 0x62               ; byte 5: (discarded)
        call    io_delay

        ; Clamp raw position to valid range [0, 1999]
        cmp     dx, 0x0000              ; is position negative? (signed comparison)
        jnl     .not_negative           ; no -> proceed
        mov     dx, 0x0000              ; yes -> clamp to 0
.not_negative:                          ; 0361
        cmp     dx, 0x07D0              ; is position >= 2000?
        jl      .not_over               ; no -> proceed
        mov     dx, 0x07CF              ; yes -> clamp to 1999
.not_over:                              ; 036A
        mov     [raw_cursor_pos], dx    ; store the raw linear position

        ; Convert linear position to (row, column)
        ; Division: position / 80 -> row (quotient), column (remainder)
        mov     ax, dx                  ; AX = clamped linear position
        mov     dx, 0x0000              ; DX:AX = 32-bit dividend (high word = 0)
        mov     bx, 0x0050              ; BX = 80 (columns per text row)
        div     bx                      ; AX = row (0-24), DX = column (0-79)
        mov     [cursor_x], dx         ; store column
        mov     [cursor_y], ax         ; store row
        pop     dx                      ; restore registers
        pop     bx
        pop     ax
        ret                             ; 0382

; -----------------------------------------------------------------------------
; compare_y: Compare cat's Y position to cursor's Y position
; -----------------------------------------------------------------------------
; Determines whether the cat needs to move up or down to reach the
; cursor, or if it is already vertically aligned.
;
; The cursor Y is in text rows (16 pixels each, range 0-24), while
; the cat Y is in 8-pixel rows (range 0-46). Multiplying cursor_y
; by 2 converts text rows to 8-pixel rows for comparison.
;
; Output:
;   CF=0, AX=0: cursor is above cat  -> cat should move UP   (state 0)
;   CF=0, AX=1: cursor is below cat  -> cat should move DOWN (state 1)
;   CF=1:       cat is vertically aligned with cursor
;
; Side effects when not aligned:
;   idle_counter reset to 0, flag_dir_changed cleared
; -----------------------------------------------------------------------------
compare_y:                              ; 0383
        mov     ax, [cursor_y]          ; AX = cursor row (0-24, text rows)
        add     ax, ax                  ; AX *= 2 -> convert to 8-pixel rows (0-48)
        cmp     ax, [cat_y]             ; compare cursor Y (8px) to cat Y (8px)
        jz      .y_aligned              ; equal -> vertically aligned

        ; Not aligned: cat needs to move vertically
        mov     word [idle_counter], 0x0000  ; reset idle counter (cat is active)
        mov     byte [flag_dir_changed], 0x00  ; clear scratch flag
        jnc     .cursor_below           ; cursor_y*2 >= cat_y? -> cursor is below

        ; Cursor is above the cat
        mov     ax, 0x0000              ; direction 0 = move UP
        clc                             ; CF=0: not yet aligned
        ret

.cursor_below:                          ; 03A0
        ; Cursor is below the cat
        mov     ax, 0x0001              ; direction 1 = move DOWN
        clc                             ; CF=0: not yet aligned
        ret

.y_aligned:                             ; 03A5
        stc                             ; CF=1: vertically aligned
        ret

; -----------------------------------------------------------------------------
; compare_x: Compare cat's X position to cursor's X position
; -----------------------------------------------------------------------------
; Determines whether the cat needs to move left or right to reach the
; cursor, or if it is already horizontally aligned.
;
; The comparison accounts for the cat_speed offset (set by -o flag):
;   - If cursor is in right half of screen (column >= 40):
;       Target X = cursor_x - 3 - cat_speed
;       (cat approaches from the right, stopping short)
;   - If cursor is in left half (column < 40):
;       Target X = cursor_x + cat_speed
;       (cat approaches from the left, stopping short)
;
; The "- 3" in the right-half case accounts for the sprite width
; (the cat sprite is 4 bytes = 32 pixels wide, so subtracting 3
; places the cat's left edge 3 columns before the cursor).
;
; Output:
;   CF=0, AX=3: target is left of cat  -> cat should move LEFT  (state 3)
;   CF=0, AX=2: target is right of cat -> cat should move RIGHT (state 2)
;   CF=1:       cat is horizontally aligned with target
;
; Side effects when not aligned:
;   idle_counter reset to 0, flag_dir_changed cleared
; -----------------------------------------------------------------------------
compare_x:                              ; 03A7
        mov     ax, [cursor_x]          ; AX = cursor column (0-79)
        cmp     ax, 0x0028              ; column >= 40? (right half of screen)
        jl      .cursor_left_half       ; no -> cursor is in left half

        ; Cursor in right half: approach from the right
        sub     ax, 0x0003              ; AX -= 3 (account for sprite width)
        sub     ax, [cat_speed]         ; AX -= cat_speed (stopping distance)
        jmp     short .do_compare

.cursor_left_half:                      ; 03B8
        ; Cursor in left half: approach from the left
        add     ax, [cat_speed]         ; AX += cat_speed (stopping distance)

.do_compare:                            ; 03BC
        cmp     ax, [cat_x]             ; compare target X to cat X
        jz      .x_aligned              ; equal -> horizontally aligned

        ; Not aligned: cat needs to move horizontally
        mov     word [idle_counter], 0x0000  ; reset idle counter

        jnc     .target_right           ; target >= cat_x? -> target is to the right

        ; Target is to the LEFT of the cat
        mov     ax, 0x0003              ; direction 3 = move LEFT
        mov     byte [flag_dir_changed], 0x00  ; clear scratch flag
        clc                             ; CF=0: not aligned
        ret

.target_right:                          ; 03D4
        ; Target is to the RIGHT of the cat
        mov     ax, 0x0002              ; direction 2 = move RIGHT
        mov     byte [flag_dir_changed], 0x00  ; clear scratch flag
        clc                             ; CF=0: not aligned
        ret

.x_aligned:                             ; 03DE
        stc                             ; CF=1: horizontally aligned
        ret

; -----------------------------------------------------------------------------
; decide_direction: Determine which direction the cat should move
; -----------------------------------------------------------------------------
; This is the cat's "brain" -- the AI logic that decides its behavior
; each frame. It uses a random coin flip to choose whether to
; prioritize horizontal or vertical movement, creating a more natural
; diagonal-looking movement pattern.
;
; The flow:
;   1. Call PRNG for a random number
;   2. If even: try horizontal movement first, then vertical
;      If odd:  try vertical movement first, then horizontal
;   3. If the cat has reached the target in both axes, enter idle mode:
;      a. If not yet bored: sit (face toward cursor)
;      b. If bored (idle_counter >= idle_threshold):
;         - Maybe scratch (1/8 chance, or if already scratched)
;         - Otherwise: pick a random wander target and walk there
;   4. When wander target is reached, return to chasing cursor
; -----------------------------------------------------------------------------
decide_direction:                       ; 03E0
        call    far [rng_ptr_off]       ; call PRNG -> AX = random 16-bit number
        test    ax, 0x0001              ; check least significant bit
        jnz     .try_y_first            ; odd -> prioritize vertical movement

; --- Even random: try horizontal first ---
.try_x_first:                           ; 03E9
        call    compare_x               ; check horizontal distance to target
        jc      .x_done_try_y           ; CF=1: aligned horizontally -> try Y
        mov     [anim_state], ax        ; not aligned: set direction (2 or 3)
        ret
.x_done_try_y:                          ; 03F2
        call    compare_y               ; check vertical distance to target
        jc      .both_aligned           ; CF=1: both aligned -> idle behavior
        mov     [anim_state], ax        ; not aligned: set direction (0 or 1)
        ret

; --- Odd random: try vertical first ---
.try_y_first:                           ; 03FB
        call    compare_y               ; check vertical distance to target
        jc      .y_done_try_x           ; CF=1: aligned vertically -> try X
        mov     [anim_state], ax        ; not aligned: set direction (0 or 1)
        ret
.y_done_try_x:                          ; 0404
        call    compare_x               ; check horizontal distance to target
        jc      .both_aligned           ; CF=1: both aligned -> idle behavior
        mov     [anim_state], ax        ; not aligned: set direction (2 or 3)
        ret

; --- Cat has reached the target (cursor or wander point) ---
.both_aligned:                          ; 040D
        cmp     byte [flag_wander], 0x00  ; was the cat in wander mode?
        jnz     .end_wander             ; yes -> stop wandering, resume chase

        ; === Idle behavior: cat is sitting at the cursor position ===

        ; Check if we have been idle long enough to get bored
        mov     ax, [idle_threshold]    ; AX = boredom threshold (default 100)
        cmp     [idle_counter], ax      ; have we waited long enough?
        jnl     .idle_timeout           ; yes -> time to scratch or wander

        ; Still waiting: just sit in place
        inc     word [idle_counter]     ; count another idle tick

        ; Choose sitting direction based on cursor position
        cmp     word [cursor_x], 0x0028 ; is cursor in left half? (col < 40)
        jnl     .sit_right              ; no -> cursor is right or center

        ; Cursor is to the left
        mov     word [anim_state], 0x0005  ; state 5 = sitting, facing left
        ret

.sit_right:                             ; 042F
        ; Cursor is to the right
        mov     word [anim_state], 0x0004  ; state 4 = sitting, facing right
        ret

.idle_timeout:                          ; 0436
        ; Cat has been sitting long enough -- do something!
        cmp     byte [flag_dir_changed], 0x00  ; have we scratched before?
        jnz     .start_scratch          ; yes -> scratch again immediately

        ; Haven't scratched yet: random chance to start
        call    far [rng_ptr_off]       ; get random number
        test    ax, 0x0007              ; check bottom 3 bits
        jz      .start_wander           ; all zero (1/8 chance) -> wander instead

.start_scratch:                         ; 0446
        ; Begin scratching animation
        mov     byte [flag_wander], 0x00     ; clear wander mode
        mov     word [anim_state], 0x0006    ; state 6 = scratching
        mov     byte [flag_dir_changed], 0x01 ; remember that we scratched
        ret

.start_wander:                          ; 0457
        ; Pick a random screen position and walk toward it
        mov     byte [flag_wander], 0x01     ; activate wander mode

        ; Generate random target X (0 to 79)
        call    far [rng_ptr_off]       ; random number in AX
        mov     dx, 0x0000              ; DX:AX = 32-bit dividend
        mov     bx, 0x0050              ; BX = 80 (number of columns)
        div     bx                      ; DX = AX mod 80 (random column)
        mov     [wander_target_x], dx   ; store random target X

        ; Generate random target Y (0 to 23)
        call    far [rng_ptr_off]       ; another random number
        mov     dx, 0x0000
        mov     bx, 0x0018              ; BX = 24 (number of text rows)
        div     bx                      ; DX = AX mod 24 (random row)
        mov     [wander_target_y], dx   ; store random target Y
        ret

.end_wander:                            ; 047D
        ; Wander target reached: go back to chasing the real cursor
        mov     byte [flag_wander], 0x00     ; deactivate wander mode
        ret

; -----------------------------------------------------------------------------
; set_wander_cursor: Override cursor position with wander target
; -----------------------------------------------------------------------------
; When the cat is in wander mode (flag_wander = 1), this function
; replaces the cursor_x and cursor_y values with the random wander
; target coordinates. This way, the normal chase logic (compare_x,
; compare_y) will move the cat toward the wander target instead of
; the actual cursor.
; -----------------------------------------------------------------------------
set_wander_cursor:                      ; 0483
        mov     ax, [wander_target_x]   ; AX = random target column
        mov     [cursor_x], ax          ; pretend cursor is at target X
        mov     ax, [wander_target_y]   ; AX = random target row
        mov     [cursor_y], ax          ; pretend cursor is at target Y
        ret                             ; 048F

; -----------------------------------------------------------------------------
; advance_animation: Advance animation frame and move the cat
; -----------------------------------------------------------------------------
; Called once per animation update. Two things happen:
;
; 1. The animation sub-frame counter cycles: 0 -> 1 -> 2 -> 3 -> 0
;    This selects which of the 4 frames in the current state to display,
;    creating the walking/scratching animation.
;
; 2. The cat moves one unit in its current direction:
;      State 0 (up):    cat_y decremented (cat moves up 8 pixels)
;      State 1 (down):  cat_y incremented (cat moves down 8 pixels)
;      State 2 (right): cat_x incremented (cat moves right 8 pixels)
;      State 3 (left):  cat_x decremented (cat moves left 8 pixels)
;      State 4-6:       no movement (sitting or scratching)
; -----------------------------------------------------------------------------
advance_animation:                      ; 0490
        inc     word [anim_frame]       ; advance to next frame
        and     word [anim_frame], 0x0003  ; wrap around: keep in range 0-3

        mov     ax, [anim_state]        ; AX = current animation state
        cmp     ax, 0x0004              ; state >= 4?
        jnl     .no_movement            ; yes (sitting/scratching) -> don't move

        ; Moving states: advance position by 1 unit
        cmp     ax, 0x0000              ; state 0: moving up?
        jg      .not_up                 ; no -> check next
        dec     word [cat_y]            ; move up: Y -= 1 (8 pixels up)
        jmp     short .no_movement
.not_up:                                ; 04AD
        cmp     ax, 0x0001              ; state 1: moving down?
        jg      .not_down               ; no -> check next
        inc     word [cat_y]            ; move down: Y += 1 (8 pixels down)
        jmp     short .no_movement
.not_down:                              ; 04B8
        cmp     ax, 0x0002              ; state 2: moving right?
        jg      .not_right              ; no -> must be state 3 (left)
        inc     word [cat_x]            ; move right: X += 1 (8 pixels right)
        jmp     short .no_movement
.not_right:                             ; 04C3
        ; State 3: moving left
        dec     word [cat_x]            ; move left: X -= 1 (8 pixels left)
.no_movement:                           ; 04C7
        ret

; -----------------------------------------------------------------------------
; update_cat: Perform one complete animation update
; -----------------------------------------------------------------------------
; The main per-tick update function, called from the VSYNC ISR.
; Each VSYNC tick, it decrements the tick_counter. When the counter
; reaches zero, a full animation step is performed:
;
;   1. Select VRAM display page 0 (port 0A6h)
;   2. Optionally show the GDC text cursor (INT 18h AH=40h)
;   3. Read cursor position (real or wander target)
;   4. Decide movement direction via AI logic
;   5. Look up the correct sprite frame
;   6. Draw the cat (erase old + draw new)
;   7. Advance animation frame and move cat position
;   8. Reload tick_counter from tick_rate
;
; If the counter has not reached zero, only step 1 is performed
; (ensuring the display page stays correct) and the function returns.
; -----------------------------------------------------------------------------
update_cat:                             ; 04C8
        mov     al, 0x00
        out     0xA6, al               ; select VRAM display page 0
                                        ;   (PC-98 supports dual-page VRAM;
                                        ;    writing 0 to port A6h selects page 0)

        dec     word [tick_counter]     ; count down one VSYNC tick
        jnz     .done                   ; not zero yet -> return (wait longer)

        ; === Tick counter reached zero: perform animation update ===

        ; Optionally show the GDC text cursor
        cmp     byte [flag_show_cursor], 0x00  ; -g flag set?
        jz      .no_cursor_show         ; no -> skip
        mov     ah, 0x40               ; INT 18h AH=40h: start text cursor display
        int     0x18                    ; PC-98 BIOS: enable hardware text cursor

.no_cursor_show:                        ; 04DD
        ; Read cursor/target position
        cmp     byte [flag_wander], 0x00  ; is cat wandering to random spot?
        jnz     .use_wander_pos         ; yes -> use wander target
        call    read_cursor_pos         ; read real cursor from PPI hardware
        jmp     short .pos_ready
.use_wander_pos:                        ; 04E9
        call    set_wander_cursor       ; override cursor with wander target

.pos_ready:                             ; 04EC
        call    decide_direction        ; AI: choose movement direction
        call    lookup_sprite           ; SI = pointer to correct sprite frame
        call    draw_cat                ; erase old cat, draw new cat
        call    advance_animation       ; advance frame + move cat position

        ; Reload the tick counter for the next update cycle
        mov     ax, [tick_rate]         ; AX = ticks between updates
        mov     [tick_counter], ax      ; reset countdown

.done:                                  ; 04FE
        ret

; =============================================================================
; SECTION 4: INTERRUPT SERVICE ROUTINES
; =============================================================================

; -----------------------------------------------------------------------------
; vsync_isr: VSYNC interrupt handler (INT 0Ah / IRQ2)
; -----------------------------------------------------------------------------
; This is the core of the TSR. The PC-9801 generates IRQ2 at each
; vertical retrace (~56 Hz in normal mode, ~31 Hz in hi-res mode).
; This handler is installed in place of the original INT 0Ah vector.
;
; Architecture:
;   - Saves minimal state (AX, DS, ES) on the interrupted program's stack
;   - Checks a reentrance guard to prevent recursive invocation
;   - Switches to a private stack (the zero-filled variable area)
;     to avoid overflowing the interrupted program's stack
;   - Sends EOI early so other interrupts can fire while we draw
;   - Calls update_cat for the actual animation work
;   - Restores the original stack and chains to the original handler
;
; The stack switch is critical: update_cat calls many subroutines and
; could need significant stack space, which the interrupted program
; might not have available.
; -----------------------------------------------------------------------------
vsync_isr:                              ; 04FF
        push    ax                      ; save AX on interrupted program's stack
        push    ds                      ; save DS
        push    es                      ; save ES
        mov     ax, cs                  ; AX = our code segment
        mov     ds, ax                  ; DS = CS (access our variables)
        mov     es, ax                  ; ES = CS (for string operations)

        ; Check reentrance guard
        cmp     byte [reentrance_guard], 0x00  ; already inside our ISR?
        jnz     .skip_update            ; yes -> don't re-enter, just chain

        ; Mark as entered
        mov     byte [reentrance_guard], 0x01  ; set reentrance lock

        ; Switch to private stack
        mov     [saved_sp], sp          ; save interrupted program's SP
        mov     [saved_ss], ss          ; save interrupted program's SS
        mov     ss, ax                  ; SS = our segment
        mov     sp, saved_sp            ; SP = 0x0234 (top of our stack area)
                                        ;   Stack grows DOWN through the zero-filled
                                        ;   region (0x0134 to 0x0233), giving us
                                        ;   256 bytes of stack space.

        ; Send End-Of-Interrupt early
        mov     al, 0x20               ; EOI command
        out     0x00, al               ; send to master PIC port 00h
                                        ;   This allows other IRQs to fire while
                                        ;   we spend time drawing sprites.
        sti                             ; re-enable interrupts

        ; Save all remaining registers on our private stack
        push    bx
        push    cx
        push    dx
        push    si
        push    di
        push    bp
        cld                             ; clear direction flag (REP goes forward)

        ; === Do the actual animation update ===
        call    update_cat              ; 052D

        ; Done with update
        cli                             ; disable interrupts for cleanup
        mov     al, 0x20
        out     0x00, al               ; send another EOI (safety measure)
        sti                             ; briefly re-enable

        ; Restore registers
        pop     bp
        pop     di
        pop     si
        pop     dx
        pop     cx
        pop     bx

        ; Switch back to interrupted program's stack
        cli                             ; disable interrupts during stack switch
        mov     sp, [saved_sp]          ; restore original SP
        mov     ax, [saved_ss]          ; get original SS
        mov     ss, ax                  ; restore original SS
        mov     byte [reentrance_guard], 0x00  ; clear reentrance lock

.skip_update:                           ; 054B
        ; Chain to the original INT 0Ah handler
        pushf                           ; push flags (simulate INT instruction)
        call    far [saved_int0a_off]   ; call original handler (far call)
        pop     es                      ; restore ES
        pop     ds                      ; restore DS
        pop     ax                      ; restore AX
        out     0x64, al               ; write to port 64h (interrupt acknowledge;
                                        ;   clears edge-triggered IRQ on PC-98)
        iret                            ; 0555: return from interrupt

; -----------------------------------------------------------------------------
; keyboard_isr: INT 18h (keyboard BIOS) passthrough handler
; -----------------------------------------------------------------------------
; A thin wrapper around the original INT 18h handler. This hook exists
; so that the TSR "owns" the INT 18h vector, allowing clean removal:
; when uninstalling, we can check if INT 18h still points to us (and
; thus is safe to unhook).
;
; The handler simply chains to the original, then acknowledges the
; interrupt via port 64h.
; -----------------------------------------------------------------------------
keyboard_isr:                           ; 0556
        pushf                           ; push flags (simulate INT)
        call    far [cs:saved_int18_off] ; call original INT 18h handler
                                        ;   (CS: override because DS may not be ours)
        out     0x64, al               ; acknowledge interrupt
        iret                            ; 055E: return from interrupt

; -----------------------------------------------------------------------------
; break_isr: INT 1Bh (break / Ctrl-C) handler
; -----------------------------------------------------------------------------
; Wraps the original INT 1Bh (break) handler. The key concern is that
; the break handler might call BIOS routines which could trigger
; another VSYNC interrupt, causing reentrancy issues.
;
; Solution: temporarily mask IRQ2 (VSYNC) in the PIC while the
; original break handler runs, then restore the mask afterward.
;
; Additionally, this handler patches the flags from the original
; handler's IRET into the caller's stack frame, so the break-handled
; status (carry flag) is properly propagated to the interrupted program.
;
; Stack frame (from caller's perspective):
;   [SP+0] = return IP
;   [SP+2] = return CS
;   [SP+4] = return FLAGS  <-- we patch this
; -----------------------------------------------------------------------------
break_isr:                              ; 055F
        mov     [cs:saved_sp_1b], sp    ; save caller's SP (for flags patching)
        mov     [cs:saved_bp_1b], bp    ; save caller's BP (we use it as pointer)

        ; Mask VSYNC interrupt to prevent reentrancy
        push    ax
        in      al, 0x02               ; read PIC mask register
        mov     [cs:saved_pic_mask], al ; save current mask
        or      al, 0x04               ; set bit 2: mask IRQ2 (disable VSYNC)
        out     0x02, al               ; apply new mask
        pop     ax

        ; Call original break handler
        pushf                           ; push flags (simulate INT)
        call    far [cs:saved_int1b_off] ; call original INT 1Bh handler

        ; Patch caller's FLAGS with the result from original handler
        push    ax                      ; save AX
        pushf                           ; push result flags onto stack
        pop     ax                      ; AX = flags from original handler
        mov     bp, [cs:saved_sp_1b]    ; BP = caller's original SP
        mov     [bp+0x04], ax           ; overwrite caller's FLAGS on stack
                                        ;   (stack layout: IP at +0, CS at +2,
                                        ;    FLAGS at +4)
        mov     bp, [cs:saved_bp_1b]    ; restore original BP

        ; Restore PIC mask (re-enable VSYNC if it was enabled before)
        mov     al, [cs:saved_pic_mask] ; get saved mask
        cli                             ; disable interrupts during PIC write
        out     0x02, al               ; restore PIC mask register
        out     0x64, al               ; acknowledge interrupt
        pop     ax                      ; restore AX
        iret                            ; 0595: return from interrupt

; =============================================================================
; SECTION 5: PRNG SEED
; =============================================================================
; Two bytes used as the seed/state for the pseudo-random number generator.
; Placed here (between the ISR code and the blank sprite) so it is
; in the resident portion of memory. The PRNG function at 0x3118
; accesses this via CS: segment overrides.
; =============================================================================

rng_seed:                               ; 0596
        dw      0x0000                  ; 16-bit PRNG seed (modified at runtime)

; =============================================================================
; SECTION 6: BLANK SPRITE (384 bytes of zeros)
; =============================================================================
; This is a sprite-sized block of all zeros, used by erase_old_sprite
; to cleanly remove the cat from VRAM. Drawing this "sprite" at the
; cat's old position overwrites the pixels with black (color 0 on all
; three planes), effectively erasing the image.
;
; Size: 32 rows x 4 bytes x 3 planes = 384 bytes (0x180)
; =============================================================================

blank_sprite:                           ; 0598
        times 384 db 0x00              ; 384 zero bytes (0x0598 through 0x0717)

; =============================================================================
; SECTION 7: SPRITE DATA (10752 bytes)
; =============================================================================
; Contains the bitmap data for all 28 animation frames (7 states x 4
; frames each). Each frame is a 32x32 pixel image stored as three
; consecutive plane bitmaps.
;
; Each plane bitmap:
;   32 rows x 4 bytes per row = 128 bytes
;   Each byte represents 8 horizontal pixels (MSB = leftmost)
;   1 = pixel on (that color), 0 = pixel off (transparent/black)
;
; Each complete frame:
;   Blue plane  (128 bytes) + Red plane (128 bytes) + Green plane (128 bytes)
;   = 384 bytes (0x180)
;
; State layout (1536 bytes = 0x600 per state):
;   Offset 0x0000: State 0 - Moving up      (4 frames)
;   Offset 0x0600: State 1 - Moving down    (4 frames)
;   Offset 0x0C00: State 2 - Moving right   (4 frames)
;   Offset 0x1200: State 3 - Moving left    (4 frames)
;   Offset 0x1800: State 4 - Sitting right  (4 frames)
;   Offset 0x1E00: State 5 - Sitting left   (4 frames)
;   Offset 0x2400: State 6 - Scratching     (4 frames)
;
; Total: 7 x 1536 = 10752 bytes (0x2A00)
; Address range: 0x0718 through 0x3117
;
; (Binary sprite data omitted for readability. In the original COM file,
;  these 10752 bytes contain the actual pixel bitmaps of the cat in
;  various poses. The cat is drawn in white/light colors against the
;  cleared VRAM background.)
; =============================================================================

sprite_data:                            ; 0718

        ; State 0: Moving Up - Frame 0
        ; (blue plane: 128 bytes, red plane: 128 bytes, green plane: 128 bytes)
        incbin  "NEKO_FIXED.COM", 0x0618, 384
        ; State 0: Moving Up - Frame 1
        incbin  "NEKO_FIXED.COM", 0x0798, 384
        ; State 0: Moving Up - Frame 2
        incbin  "NEKO_FIXED.COM", 0x0918, 384
        ; State 0: Moving Up - Frame 3
        incbin  "NEKO_FIXED.COM", 0x0A98, 384

        ; State 1: Moving Down - Frame 0
        incbin  "NEKO_FIXED.COM", 0x0C18, 384
        ; State 1: Moving Down - Frame 1
        incbin  "NEKO_FIXED.COM", 0x0D98, 384
        ; State 1: Moving Down - Frame 2
        incbin  "NEKO_FIXED.COM", 0x0F18, 384
        ; State 1: Moving Down - Frame 3
        incbin  "NEKO_FIXED.COM", 0x1098, 384

        ; State 2: Moving Right - Frame 0
        incbin  "NEKO_FIXED.COM", 0x1218, 384
        ; State 2: Moving Right - Frame 1
        incbin  "NEKO_FIXED.COM", 0x1398, 384
        ; State 2: Moving Right - Frame 2
        incbin  "NEKO_FIXED.COM", 0x1518, 384
        ; State 2: Moving Right - Frame 3
        incbin  "NEKO_FIXED.COM", 0x1698, 384

        ; State 3: Moving Left - Frame 0
        incbin  "NEKO_FIXED.COM", 0x1818, 384
        ; State 3: Moving Left - Frame 1
        incbin  "NEKO_FIXED.COM", 0x1998, 384
        ; State 3: Moving Left - Frame 2
        incbin  "NEKO_FIXED.COM", 0x1B18, 384
        ; State 3: Moving Left - Frame 3
        incbin  "NEKO_FIXED.COM", 0x1C98, 384

        ; State 4: Sitting Right - Frame 0
        incbin  "NEKO_FIXED.COM", 0x1E18, 384
        ; State 4: Sitting Right - Frame 1
        incbin  "NEKO_FIXED.COM", 0x1F98, 384
        ; State 4: Sitting Right - Frame 2
        incbin  "NEKO_FIXED.COM", 0x2118, 384
        ; State 4: Sitting Right - Frame 3
        incbin  "NEKO_FIXED.COM", 0x2298, 384

        ; State 5: Sitting Left - Frame 0
        incbin  "NEKO_FIXED.COM", 0x2418, 384
        ; State 5: Sitting Left - Frame 1
        incbin  "NEKO_FIXED.COM", 0x2598, 384
        ; State 5: Sitting Left - Frame 2
        incbin  "NEKO_FIXED.COM", 0x2718, 384
        ; State 5: Sitting Left - Frame 3
        incbin  "NEKO_FIXED.COM", 0x2898, 384

        ; State 6: Scratching - Frame 0
        incbin  "NEKO_FIXED.COM", 0x2A18, 384
        ; State 6: Scratching - Frame 1
        incbin  "NEKO_FIXED.COM", 0x2B98, 384
        ; State 6: Scratching - Frame 2
        incbin  "NEKO_FIXED.COM", 0x2D18, 384
        ; State 6: Scratching - Frame 3
        incbin  "NEKO_FIXED.COM", 0x2E98, 384

; =============================================================================
; SECTION 8: PSEUDO-RANDOM NUMBER GENERATOR
; =============================================================================
; A simple 16-bit PRNG used for the cat's behavioral decisions (which
; direction to try first, when to scratch, where to wander).
;
; Called via a FAR pointer stored at [rng_ptr_off:rng_ptr_seg], which
; allows multiple resident copies (-f flag) to either share the same
; PRNG or each have their own.
;
; All memory accesses use CS: segment overrides so the function works
; regardless of the caller's DS setting.
;
; Algorithm:
;   1. XOR the 16-bit seed with the constant 0x6789
;   2. Square the seed: seed * seed (keep low 16 bits as product)
;   3. Store AH (bits 8-15 of product) as new seed low byte
;   4. Store DL (bits 16-23 of product) as new seed high byte
;   5. Return the new seed value in AX
;
; This is a variant of the "middle-square" PRNG method with an XOR
; step to prevent fixed-point convergence at zero.
;
; Ends with RETF (far return) since it is called via CALL FAR.
; =============================================================================

prng_function:                          ; 3118
        xor     word [cs:rng_seed], 0x6789   ; step 1: XOR seed with constant
        mov     ax, [cs:rng_seed]       ; AX = modified seed
        mul     word [cs:rng_seed]      ; DX:AX = seed * seed (32-bit result)
        mov     [cs:rng_seed], ah       ; store middle byte of result as seed low
        mov     [cs:rng_seed+1], dl     ; store high byte as seed high
        mov     ax, [cs:rng_seed]       ; AX = new seed value (returned to caller)
        retf                            ; 3136: far return

; =============================================================================
; SECTION 9: DOS STRINGS
; =============================================================================
; Dollar-terminated strings used by the setup code for user messages.
; Printed via INT 21h AH=09h (DOS print string function).
;
; IMPORTANT NOTE ON STRING REFERENCES:
; This binary (NEKO_FIXED.COM) has been patched after the original
; compilation. Some bytes were inserted in the code region between
; the str_banner reference and the later strings, causing a cumulative
; offset in the string pointers. As a result, the code's "mov dx"
; references point 2-5 bytes INTO the strings, truncating the "Neko"
; prefix. The actual strings in the data are correct; only the code
; references are slightly off:
;
;   str_banner      -> 0x3137 (correct, prints full string)
;   str_removed     -> 0x3172 (should be 0x3170; prints "ko.com removed.")
;   str_already     -> 0x3186 (should be 0x3183; prints "o.com already staying.")
;   str_not_staying -> 0x31A2 (should be 0x319E; prints ".com is not staying now.")
;   str_cant_remove -> 0x31C1 (should be 0x31BC; prints " program is using...")
;   str_too_many    -> 0x320A (should be 0x3206; prints "many cats.")
;
; This is a cosmetic bug in the FIXED binary. The labels below use
; the actual string start addresses; the code references in Section 10
; use the (slightly wrong) addresses found in the binary.
; =============================================================================

str_banner:                             ; 3137
        db      "Neko.com version 0.70", 0x0A
        db      "Copyright(c) by <tenten> & naoshi", 0x0A, "$"
                                        ; Displayed on every invocation
                                        ; Code ref: mov dx, 0x3137 (correct)

str_removed:                            ; 3170
        db      "Neko.com removed.", 0x0A, "$"
                                        ; Printed after successful -r removal
                                        ; Code ref: mov dx, 0x3172 (2 bytes into string)

str_already:                            ; 3183
        db      "Neko.com already staying.", 0x0A, "$"
                                        ; Printed if already resident (without -f)
                                        ; Code ref: mov dx, 0x3186 (3 bytes into string)

str_not_staying:                        ; 319E
        db      "Neko.com is not staying now.", 0x0A, "$"
                                        ; Printed if -r used but not resident
                                        ; Code ref: mov dx, 0x31A2 (4 bytes into string)

str_cant_remove:                        ; 31BC
        db      "Other program is using VSYNC or BIOS vector.", 0x0A
        db      "Neko.com cannot remove now.", 0x0A, "$"
                                        ; Printed if -r fails (vectors modified)
                                        ; Code ref: mov dx, 0x31C1 (5 bytes into string)

str_too_many:                           ; 3206
        db      "Too many cats.", 0x0A, "$"
                                        ; Printed if -f exceeds the cat limit
                                        ; Code ref: mov dx, 0x320A (4 bytes into string)

; =============================================================================
; SECTION 10: SETUP / TRANSIENT CODE
; =============================================================================
; This code runs once during program startup. It handles:
;   - Printing the version banner
;   - Parsing command-line arguments
;   - Checking for existing resident copies
;   - Installing or removing the TSR
;   - Adding additional cats (-f mode)
;
; After the program goes TSR (INT 21h AH=31h), DOS frees this section
; (and everything after it) back to the memory pool. Only the code
; from 0x0100 through the resident boundary remains in memory.
; =============================================================================

; -----------------------------------------------------------------------------
; check_resident: Check if a copy of NEKO.COM is already resident
; -----------------------------------------------------------------------------
; Strategy: get the current INT 0Ah handler address, then compare
; the resident code region (0x0253 through 0x0595) byte-by-byte
; between our copy (in DS) and the copy at the INT 0Ah handler's
; segment (in ES). If all bytes match, NEKO is already installed.
;
; This works because the resident code region is identical across
; all instances of the same version of NEKO.COM.
;
; Input:  none
; Output:
;   CF=1: a copy is already resident (ES = its segment)
;   CF=0: not resident
; Clobbers: AX (via INT 21h)
; Preserves: ES on return (contains resident segment if CF=1)
; -----------------------------------------------------------------------------
                db      0x00            ; 3216: alignment/padding byte
check_resident:                         ; 3217
        push    es
        push    cx
        push    di
        push    si
        mov     ax, 0x350A              ; DOS function 35h: get interrupt vector
        int     0x21                    ;   Input: AL = 0Ah (VSYNC interrupt)
                                        ;   Output: ES:BX = current INT 0Ah handler
        mov     si, wait_vsync          ; SI = start of our resident code (0x0253)
        mov     di, si                  ; DI = same offset in ES segment
        mov     cx, rng_seed            ; CX = end marker (0x0596)
        sub     cx, si                  ; CX = number of bytes to compare (0x0343)
        repe    cmpsb                   ; compare DS:SI (our code) vs ES:DI (theirs)
        clc                             ; assume not found (CF=0)
        jnz     .not_resident           ; mismatch -> not our TSR
        stc                             ; all bytes match -> resident! (CF=1)
.not_resident:                          ; 3230
        pop     si
        pop     di
        pop     cx
        pop     es                      ; ES preserved (resident segment if CF=1)
        ret                             ; 3234

; -----------------------------------------------------------------------------
; to_lowercase: Convert ASCII character to lowercase
; -----------------------------------------------------------------------------
; Input:  AL = character
; Output: AL = lowercase if was A-Z, unchanged otherwise
; -----------------------------------------------------------------------------
to_lowercase:                           ; 3235
        cmp     al, 'A'                ; 0x41
        jl      .done
        cmp     al, 'Z'                ; 0x5A
        jg      .done
        add     al, 0x20               ; convert A-Z to a-z
.done:                                  ; 323F
        ret

; -----------------------------------------------------------------------------
; scan_cmdline: Scan the command line tail for a specific -X flag
; -----------------------------------------------------------------------------
; Searches the PSP command tail (at offset 80h in the PSP) for the
; pattern "-X" where X matches the character in DL (case-insensitive).
;
; The PSP command tail format:
;   Byte at 80h: length of command tail
;   Bytes 81h+:  the command tail text (space-separated arguments)
;
; Input:  DL = flag letter to look for (must be lowercase)
; Output:
;   CF=0: flag found
;   CF=1: flag not found
; Clobbers: AX, CX, DI
; -----------------------------------------------------------------------------
scan_cmdline:                           ; 3240
        mov     di, 0x0080              ; DI -> PSP command tail length byte
        mov     cl, [bp+si]            ; CL = command tail length
                                        ;   (BP and SI are 0 at COM entry,
                                        ;    so [BP+SI] = [0000h] but since
                                        ;    DS=PSP segment, this reads [DS:0080h]
                                        ;    ... actually [BP+SI] with BP=0,SI=0
                                        ;    would be [0], which is the INT 20h
                                        ;    instruction at PSP:0000. This appears
                                        ;    to be a quirk -- the code actually
                                        ;    reads the length from [DS:0080h] via
                                        ;    some register setup not shown here.)
        inc     di                      ; DI -> first byte of command tail (81h)
        xor     ch, ch                  ; CX = length of command tail

        mov     al, '-'                ; character to scan for
.scan_loop:                             ; 324A
        repne   scasb                   ; scan for next '-' in ES:DI
        jnz     .not_found              ; reached end without finding '-'
        push    ax                      ; save AX
        mov     al, [di]               ; AL = character after the '-'
        call    to_lowercase            ; convert to lowercase
        cmp     al, dl                  ; matches our flag letter?
        pop     ax                      ; restore AX
        jnz     .scan_loop             ; no match -> keep scanning
        clc                             ; found! CF=0
        ret                             ; 325A
.not_found:                             ; 325B
        stc                             ; not found: CF=1
        ret                             ; 325C

; -----------------------------------------------------------------------------
; try_remove: Attempt to remove the resident copy from memory
; -----------------------------------------------------------------------------
; Checks that it is safe to unhook our interrupt vectors (no other TSR
; has hooked them after us), then restores all original vectors and
; frees the resident copy's memory.
;
; Safety check: INT 18h and INT 0Ah must both point to the same
; segment (ours). If they point to different segments, another TSR
; has hooked one of them and we cannot safely unhook.
;
; Input:  none (assumes check_resident was called and succeeded)
; Output:
;   CF=0: successfully removed
;   CF=1: cannot remove (vectors modified by another program)
; -----------------------------------------------------------------------------
try_remove:                             ; 325D
        push    ds
        push    es

        ; Verify both vectors still point to us
        mov     ax, 0x3518              ; get INT 18h vector
        int     0x21                    ; ES:BX
        mov     dx, es                  ; DX = INT 18h segment

        mov     ax, 0x350A              ; get INT 0Ah vector
        int     0x21                    ; ES:BX
        mov     ax, es                  ; AX = INT 0Ah segment

        cmp     ax, dx                  ; both in same segment?
        jnz     .unsafe                 ; different segments -> can't remove

        ; Both vectors point to our segment. Proceed with removal.
        mov     ds, ax                  ; DS = resident copy's segment

        ; Restore original INT 18h vector
        push    ds
        lds     dx, [saved_int18_off]   ; DS:DX = original INT 18h handler
        mov     ax, 0x2518              ; DOS: set INT 18h vector
        int     0x21
        pop     ds

        ; Restore original INT 0Ah vector
        push    ds
        lds     dx, [saved_int0a_off]   ; DS:DX = original INT 0Ah handler
        mov     ax, 0x250A              ; DOS: set INT 0Ah vector
        int     0x21
        pop     ds

        ; Restore original INT 1Bh vector
        push    ds
        lds     dx, [saved_int1b_off]   ; DS:DX = original INT 1Bh handler
        mov     ax, 0x251B              ; DOS: set INT 1Bh vector
        int     0x21
        pop     ds

        ; Free resident copy's environment segment
        push    es
        mov     ax, [es:0x002C]         ; get environment segment from PSP
                                        ;   (PSP offset 2Ch = environment pointer)
        mov     es, ax                  ; ES = environment segment
        mov     ah, 0x49               ; DOS: free memory block (AH=49h)
        int     0x21                    ; free the environment
        pop     es

        ; Free the resident copy's code/data segment
        push    es
        mov     ax, es                  ; AX = resident copy's segment
        mov     es, ax                  ; ES = same (target for free)
        mov     ah, 0x49               ; DOS: free memory block
        int     0x21                    ; free the resident copy itself
        pop     es

        ; Clean up the screen (erase cat, clear VRAM, restore display)
        call    cleanup_screen

        clc                             ; success: CF=0
        pop     es
        pop     ds
        ret                             ; 32B0

.unsafe:                                ; 32B1
        ; Cannot safely remove: another TSR has hooked our vectors
        sti                             ; ensure interrupts are enabled
        jmp     short .unsafe_exit      ; jump to the return path
                                        ; (Note: this jumps to 0x32AE which is
                                        ;  the "clc; pop es; pop ds; ret" above.
                                        ;  However, since we want CF=1 for failure,
                                        ;  this actually returns CF=0. The caller
                                        ;  checks differently -- see setup code.)
                                        ; Actually examining the jump target:
                                        ;  0x32B2: EB FA -> jmp short 0x32AE
                                        ;  0x32AE is: F8 = clc, 07 = pop es, 1F = pop ds, C3 = ret
                                        ;  So this returns CF=0 (success) even on failure!
                                        ;  This appears to be a bug or the failure
                                        ;  path is handled differently in the caller.
.unsafe_exit equ $ - 2                  ; target is 2 bytes back (the clc above)

; -----------------------------------------------------------------------------
; clear_vram_plane: Fill an entire VRAM plane with zeros
; -----------------------------------------------------------------------------
; Clears 32000 bytes (640x400 / 8) of a single VRAM bitplane.
;
; Input:  AX = VRAM plane segment (0A800h, 0B000h, or 0B800h)
; Clobbers: ES, DI, CX, AX
; -----------------------------------------------------------------------------
clear_vram_plane:                       ; 32B4
        mov     di, 0x0000              ; start at offset 0
        mov     es, ax                  ; ES = plane segment
        mov     cx, 0x3E80              ; CX = 16000 words = 32000 bytes
        mov     ax, 0x0000              ; fill value = 0
        rep     stosw                   ; clear the entire plane
        ret                             ; 32C1

; -----------------------------------------------------------------------------
; cleanup_screen: Erase cat, clear VRAM, restore display settings
; -----------------------------------------------------------------------------
; Called during removal (-r) and during initial installation to set up
; a clean display state. Performs:
;   1. Hide INT 33h mouse cursor (if active)
;   2. Hide GDC text cursor (INT 18h AH=41h)
;   3. Clear all three VRAM planes to black
;   4. Show GDC text cursor (INT 18h AH=40h)
;   5. Enable graphics display (INT 18h AH=42h, CH=C0h)
;   6. Show INT 33h mouse cursor (if active)
; -----------------------------------------------------------------------------
cleanup_screen:                         ; 32C2
        ; Hide mouse cursor if using INT 33h driver
        cmp     byte [flag_mouse_drv], 0x00
        jz      .no_mouse_hide
        mov     ax, 0x0002              ; INT 33h function 2: hide cursor
        int     0x33
.no_mouse_hide:                         ; 32CE

        ; Hide the hardware text cursor
        mov     ah, 0x41               ; INT 18h AH=41h: stop cursor display
        int     0x18

        ; Clear all three VRAM bitplanes
        mov     ax, 0xA800             ; blue plane segment
        call    clear_vram_plane
        mov     ax, 0xB000             ; red plane segment
        call    clear_vram_plane
        mov     ax, 0xB800             ; green plane segment
        call    clear_vram_plane

        ; Restore display settings
        mov     ah, 0x40               ; INT 18h AH=40h: start cursor display
        int     0x18
        mov     ah, 0x42               ; INT 18h AH=42h: set display attributes
        mov     ch, 0xC0               ; CH = C0h: enable graphics display plane
        int     0x18

        ; Show mouse cursor if using INT 33h driver
        cmp     byte [flag_mouse_drv], 0x00
        jz      .no_mouse_show
        mov     ax, 0x0001              ; INT 33h function 1: show cursor
        int     0x33
.no_mouse_show:                         ; 32FA
        ret

; =============================================================================
; setup_entry: Main entry point (jumped to from 0x0100)
; =============================================================================
; This is the first code that actually executes. It initializes
; variables, parses the command line, and either installs the TSR,
; removes an existing copy, adds another cat, or exits with an error.
;
; Flow overview:
;   1. Print version banner
;   2. Initialize PRNG pointer and data segment variable
;   3. Detect high-resolution display mode
;   4. Check for -r flag -> attempt removal
;   5. Check for -g, -m, -o flags -> set configuration
;   6. Check if already resident:
;      a. Not resident -> install (hook vectors, go TSR)
;      b. Resident + -f flag -> add another cat
;      c. Resident, no -f -> error "already staying"
; =============================================================================

; Note: there are a few bytes of preamble code (0x32FB-0x3302) that
; set up the banner print before falling through to the main entry.
; The JMP at 0x0100 targets 0x3303, but the setup actually begins
; at 0x32FB with the banner print.

setup_banner:                           ; 32FB
        mov     ah, 0x09               ; DOS function 09h: print string
        mov     dx, 0x3137              ; DX -> str_banner ("Neko.com version 0.70...")
        int     0x21                    ; print the version banner

setup_entry:                            ; 3302 (intended entry point)
        ; Save our segment for the resident code's use
        mov     [data_seg_val], ds      ; 3302: resident draw_sprite needs this
                                        ;   (stores CS value so the ISR can set DS
                                        ;    to access sprite data in our segment)

        ; Set up the PRNG far pointer
        mov     word [rng_ptr_off], prng_function  ; offset of PRNG (0x3118)
        mov     [rng_ptr_seg], cs       ; segment = CS

        ; Detect display mode
        mov     ah, 0x02               ; INT 18h AH=02h: get keyboard status
        int     0x18                    ; AL = keyboard shift state
        test    al, 0x04               ; bit 2: some mode flag
                                        ;   (On PC-98, this may indicate hi-res
                                        ;    or a specific keyboard state)
        jz      .not_hires
        mov     byte [flag_hires], 0x01 ; set high-resolution flag
.not_hires:                             ; 331D

; --- Parse command-line flags ---

        ; Check for -r (remove) flag
        mov     dl, 'r'                ; 0x72
        call    scan_cmdline            ; search command line for "-r"
        jc      .no_remove              ; not found -> skip removal path

        ; -r flag found: attempt to remove the resident copy
        call    check_resident          ; is NEKO already resident?
        jc      .resident_try_remove    ; yes -> try to remove it
        jmp     .err_not_staying        ; no -> error: nothing to remove

.resident_try_remove:                   ; 332C
        call    try_remove              ; attempt uninstallation
        jc      .err_vectors_busy       ; failed? -> error: vectors busy
        jmp     .removal_success        ; success -> print message and exit

.err_vectors_busy:                      ; 3334
        ; Removal failed: another program has hooked the vectors
        mov     ah, 0x09
        mov     dx, 0x31C1              ; -> str_cant_remove+5 (prints " program is using...")
                                        ;    (see string reference note in Section 9)
        int     0x21
        mov     ax, 0x4C03             ; exit with return code 3
        int     0x21

; --- No -r flag: parse remaining options ---
.no_remove:                             ; 3340

        ; Check for -g (GDC cursor display) flag
        mov     dl, 'g'                ; 0x67
        call    scan_cmdline
        jc      .no_g_flag
        mov     byte [flag_show_cursor], 0x01  ; enable GDC cursor during updates
.no_g_flag:                             ; 334C

        ; Check for -m (mouse driver) flag
        mov     dl, 'm'                ; 0x6D
        call    scan_cmdline
        jc      .no_m_flag
        mov     byte [flag_mouse_drv], 0x01  ; use INT 33h mouse driver
.no_m_flag:                             ; 3358

        ; Check for -o (offset/distance) flag
        mov     dl, 'o'                ; 0x6F
        call    scan_cmdline
        jc      .no_o_flag
        mov     word [cat_speed], 0x0004     ; cat stops 4 columns from cursor
.no_o_flag:                             ; 3365

        ; Check if NEKO is already resident
        call    check_resident          ; compare our code with INT 0Ah handler
        jnc     .first_install          ; not resident -> proceed with install
        jmp     .already_resident       ; resident -> try -f or report error

; =============================================================================
; First installation: hook vectors and go resident
; =============================================================================
.first_install:                         ; 336D

        ; Adjust tick rate for high-resolution mode
        cmp     byte [flag_hires], 0x00 ; hi-res mode detected?
        jz      .rate_ok
        mov     word [tick_rate], 0x0002 ; VSYNC is slower in hi-res (~31 Hz),
                                        ;   so update more frequently (every 2 ticks
                                        ;   instead of 10) to maintain speed
.rate_ok:                               ; 337A

        ; --- Hook INT 0Ah (VSYNC interrupt / IRQ2) ---
        mov     ax, 0x350A              ; DOS: get current INT 0Ah vector
        int     0x21                    ; ES:BX = current handler
        mov     [saved_int0a_off], bx   ; save original offset
        mov     [saved_int0a_seg], es   ; save original segment
        mov     dx, vsync_isr           ; DX = our VSYNC ISR (0x04FF)
        mov     ax, 0x250A              ; DOS: set INT 0Ah vector
        int     0x21                    ; INT 0Ah now points to our handler

        ; --- Hook INT 18h (keyboard BIOS) ---
        mov     ax, 0x3518              ; DOS: get current INT 18h vector
        int     0x21                    ; ES:BX = current handler
        mov     [saved_int18_off], bx   ; save original offset
        mov     [saved_int18_seg], es   ; save original segment
        mov     dx, keyboard_isr        ; DX = our keyboard wrapper (0x0556)
        mov     ax, 0x2518              ; DOS: set INT 18h vector
        int     0x21                    ; INT 18h now points to our wrapper

        ; --- Hook INT 1Bh (break handler) ---
        mov     ax, 0x351B              ; DOS: get current INT 1Bh vector
        int     0x21                    ; ES:BX = current handler
        mov     [saved_int1b_off], bx   ; save original offset
        mov     [saved_int1b_seg], es   ; save original segment
        mov     dx, break_isr           ; DX = our break wrapper (0x055F)
        mov     ax, 0x251B              ; DOS: set INT 1Bh vector
        int     0x21                    ; INT 1Bh now points to our wrapper

        ; --- Initialize screen state ---
        call    cleanup_screen          ; clear VRAM and set up display

        ; --- Enable VSYNC interrupt in the PIC ---
        in      al, 0x02               ; read master PIC mask register
        and     al, 0xFB               ; clear bit 2: unmask IRQ2 (enable VSYNC)
        out     0x02, al               ; write back to PIC
        out     0x64, al               ; acknowledge any pending interrupt

        ; --- Terminate and Stay Resident ---
        ; Choose how much memory to keep resident based on mode:
        ;   Normal:  keep up to str_banner (0x3137) -- includes sprites
        ;   -f mode: keep only up to rng_seed (0x0596) -- share sprites
        ;            with the first cat's copy

        cmp     byte [0x321E], 0x00     ; check the -f mode flag
                                        ;   (Byte at 0x321E, within the check_resident
                                        ;    function, is repurposed as a flag during
                                        ;    the -f installation path.
                                        ;    0 = first install, 1 = adding a cat.)
        jnz     .tsr_small              ; -f mode: keep less memory

        ; Normal TSR: keep everything up to the start of strings
        mov     dx, 0x3137              ; DX = end of resident region (str_banner)
        mov     cl, 4
        shr     dx, cl                  ; convert bytes to paragraphs (>> 4)
        inc     dx                      ; round up to next paragraph
        mov     ax, 0x3100              ; DOS function 31h: TSR (AH=31h, AL=00h)
        int     0x21                    ; terminate and stay resident!
                                        ;   DX paragraphs remain in memory.

.tsr_small:                             ; 33D8
        ; -f mode TSR: keep only up to the PRNG seed
        ; (sprite data is shared from the first cat's segment via data_seg_val)
        mov     dx, rng_seed            ; DX = 0x0596 (end of minimal resident region)
        mov     cl, 4
        shr     dx, cl                  ; convert to paragraphs
        inc     dx                      ; round up
        mov     ax, 0x3100              ; DOS: TSR
        int     0x21                    ; go resident with minimal footprint

; =============================================================================
; Removal success message and clean exit
; =============================================================================
.removal_success:                       ; 33E5
        mov     ah, 0x09
        mov     dx, 0x3172              ; -> str_removed+2 (prints "ko.com removed.")
                                        ;    (see string reference note in Section 9)
        int     0x21
        out     0x64, al               ; acknowledge interrupt (cleanup)
        mov     ax, 0x4C00              ; DOS: exit with return code 0 (success)
        int     0x21

; =============================================================================
; Already resident: try -f (add another cat) or report error
; =============================================================================
.already_resident:                      ; 33F3

        ; Check for -f (follow / add cat) flag
        mov     dl, 'f'                ; 0x66
        call    scan_cmdline
        jc      .err_already            ; no -f flag -> error: already staying

        ; -f flag present: add another cat to the screen!
        mov     byte [0x321E], 0x01     ; set the "adding cat" flag at 0x321E

        ; Read the existing cat's configuration from its resident copy
        mov     ax, 0x350A              ; get INT 0Ah vector (points to existing cat)
        int     0x21                    ; ES:BX = existing cat's VSYNC handler

        ; Check cat count limit (prevent too many cats)
        mov     ax, [es:cat_speed]      ; AX = existing cat's speed offset
        cmp     ax, 0x0024              ; >= 36? (max ~9 cats with +4 each)
        jnl     .err_too_many           ; yes -> "Too many cats."

        ; Increase difficulty parameters for this additional cat
        add     ax, 0x0004              ; increase speed offset by 4
        add     [cat_speed], ax         ; set our cat_speed (cumulative)

        mov     ax, [es:idle_threshold] ; get existing idle threshold
        add     ax, 0x000A              ; increase by 10 (cat waits longer before acting)
        mov     [idle_threshold], ax    ; set our idle threshold

        mov     ax, [es:tick_rate]      ; get existing tick rate
        add     ax, 0x0001              ; slow down slightly (+1 tick between updates)
        mov     [tick_rate], ax         ; set our tick rate

        ; Share the data segment and PRNG with the first cat
        ; (so all cats reference the same sprite data and RNG)
        mov     ax, [es:data_seg_val]
        mov     [data_seg_val], ax      ; use first cat's data segment

        mov     ax, [es:rng_ptr_off]
        mov     [rng_ptr_off], ax       ; use first cat's PRNG offset
        mov     ax, [es:rng_ptr_seg]
        mov     [rng_ptr_seg], ax       ; use first cat's PRNG segment

        ; Randomize this cat's initial tick rate so multiple cats
        ; don't all update in perfect synchronization
        call    far [rng_ptr_off]       ; get random number
        and     ax, 0x000F              ; mask to range 0-15
        add     ax, 0x0007              ; shift to range 7-22
        mov     [tick_rate], ax         ; set randomized tick rate

        jmp     .first_install          ; proceed with installation (hook vectors, TSR)

; =============================================================================
; Error exits
; =============================================================================

; --- Error: already resident without -f ---
.err_already:                           ; 344D
        mov     ah, 0x09
        mov     dx, 0x3186              ; -> str_already+3 (prints "o.com already staying.")
                                        ;    (see string reference note in Section 9)
        int     0x21
        mov     ax, 0x4C02              ; exit with return code 2
        int     0x21

; --- Error: too many cats (cat_speed >= 36) ---
.err_too_many:                          ; 3459
        mov     ah, 0x09
        mov     dx, 0x320A              ; -> str_too_many+4 (prints "many cats.")
                                        ;    (see string reference note in Section 9)
        int     0x21
        mov     ax, 0x4C02              ; exit with return code 2
        int     0x21

; --- Error: -r used but not resident ---
.err_not_staying:                       ; 3465
        mov     ah, 0x09
        mov     dx, 0x31A2              ; -> str_not_staying+4 (prints ".com is not staying now.")
                                        ;    (see string reference note in Section 9)
        int     0x21
        mov     ax, 0x4C02              ; exit with return code 2
        int     0x21

; --- End of file ---
        db      0x0A                    ; 3471: trailing newline (end of COM file)
