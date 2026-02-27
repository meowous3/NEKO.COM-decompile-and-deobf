# NEKO.COM — PC-9801 Reverse Engineering

Annotated disassembly and extracted sprites for the original Neko desktop pet by **\<tenten\> & naoshi** (v0.70), a TSR program for the NEC PC-9801 that displays an animated cat chasing the cursor.

The binary was sourced from [eliot-akira/neko](https://github.com/eliot-akira/neko/).

## Files

| File | Description |
|---|---|
| `NEKO.asm` | Fully annotated disassembly |
| `sprites/` | Extracted PNGs (28 sprites + spritesheet) |

## Disassembly

`NEKO.asm` is a complete annotated reverse engineering of the binary covering:

- **TSR installation** — hooks VSYNC interrupt (INT 0Ah/IRQ2) for ~56Hz updates
- **Cursor tracking** — reads PPI keyboard port or mouse interrupt for cursor position
- **State machine** — 7 animation states (up, down, right, left, scratch-right, scratch-left, sleep), 4 frames each
- **Movement** — grid-based (8px cells), random X/Y priority for diagonal feel
- **Idle behavior** — sit → scratch/groom → sleep → random wander cycle
- **VRAM rendering** — direct writes to 3 bitplanes (B/R/G) at segments A800h/B000h/B800h

## Sprites

28 sprites (7 states × 4 frames) extracted as 32×32 PNGs at original resolution, plus a combined spritesheet.

Each sprite is 384 bytes in the original binary: three 128-byte bitplanes (Blue, Red, Green) at 32 rows × 4 bytes per row. The 3-bit color index maps to the standard PC-9801 8-color palette.

### Animation states

| State | Frames | Description |
|---|---|---|
| 0 | `0_move_up_f0–f3` | Walking up |
| 1 | `1_move_down_f0–f3` | Walking down |
| 2 | `2_move_right_f0–f3` | Walking right |
| 3 | `3_move_left_f0–f3` | Walking left |
| 4 | `4_sit_right_f0–f3` | Scratching (facing right) |
| 5 | `5_sit_left_f0–f3` | Scratching (facing left) |
| 6 | `6_scratch_f0–f3` | Sleeping |
