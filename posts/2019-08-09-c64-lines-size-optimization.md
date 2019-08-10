---
title: Dirty tricks 6502 programmers use
author: Janne Hellsten
public: true
syntax-css: syntax2.css  # use a newer syntax CSS for this file
thumb: /images/c64/lines/lines-2x.png
---

TODO outline:

- Intro (duh)
  * explain memory layout, link to instruction set
  * explain the basic vic functionality like border color, charset mode
- C implementation, asm reference
- Slightly optimized asm "reference" with C too
- Assembler conventions: `$ab`, `$#ab`

Foo foo foo. Hosted a twitter compo yeah yeah.  https://gist.github.com/nurpax/96285c08710387e73ea5e039e27980af

The competition rules were simple: Make a C64 executable (PRG) that draws two lines to form the below image.  The objective is to do this in as few bytes as possible.

<img width="75%" class="img-pixelated" src="/images/c64/lines/lines-2x.png" />

Entries were posted as Twitter replies and DMs, containing only the PRG byte-length and an MD5 hash of the PRG file.

This blog post is a review of the sort of C64 coding tricks that were used in compo submissions.

## Basics

The C64 default graphics mode is the 40x25 charset mode.  The framebuffer split into two arrays in RAM:

* `$0400` (screen RAM, 40x25 bytes)
* `$d800` (color RAM, 40x25 bytes)

To set a character, you store a byte into into the `$0400` (e.g., `$0400 + y*40 + x`) buffer.  The color RAM is by default initialized to light blue (color 14) which happens to be the same color we use for the lines, so we don't have to touch the color RAM in this exercise.

You can control the border and background colors with memory mapped I/O registers at `$d020` (border) and `$d021` (background).

Drawing the two lines is pretty easy as we can hardcode for the fixed slope.  Here's a C implementation that draws the lines and dumps screen contents on stdout (register writes stubbed out and screen RAM is `malloc()`'d memory that it runs on PC):

```{.c}
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

void dump(const uint8_t* screen) {
    const uint8_t* s = screen;
    for (int y = 0; y < 25; y++) {
        for (int x = 0; x < 40; x++, s++) {
            printf("%c", *s == 0xa0 ? '#' : '.');
        }
        printf("\n");
    }
}

void setreg(uintptr_t dst, uint8_t v) {
//  *((uint8_t *)dst) = v;
}

int main() {
//  uint8_t* screenRAM = (uint_8*)0x0400;
    uint8_t* screenRAM = (uint8_t *)calloc(40*25, 0x20);

    setreg(0xd020, 0); // Set border color
    setreg(0xd021, 0); // Set background color

    int yslope = (25<<8)/40;
    int yf = yslope/2;
    for (int x = 0; x < 40; x++) {
        int yi = yf >> 8;
        // First line
        screenRAM[x + yi*40] = 0xa0;
        // Second line (X-mirrored)
        screenRAM[(39-x) + yi*40] = 0xa0;
        yf += yslope;
    }

    dump(screenRAM);
}
```

The screen codes used above are: `$20` (blank character, e.g., space) and `$a0` (8x8 filled block).  If you run it, you should see ASCII art for the two lines:

```
##....................................##
..#..................................#..
...##..............................##...
.....#............................#.....
......##........................##......
........##....................##........
..........#..................#..........
...........##..............##...........
.............#............#.............
..............##........##..............
................##....##................
..................#..#..................
...................##...................
..................#..#..................
................##....##................
..............##........##..............
.............#............#.............
...........##..............##...........
..........#..................#..........
........##....................##........
......##........................##......
.....#............................#.....
...##..............................##...
..#..................................#..
##....................................##
```

Using 6502 assembly and assembly pseudos, we can trivially implement the same in assembly:

```{.asm}
!include "c64.asm"

+c64::basic_start(entry)

entry: {
    lda #0      ; black color
    sta $d020   ; set border to 0
    sta $d021   ; set background to 0

    ; clear the screen
    ldx #0
    lda #$20
clrscr:
!for i in [0, $100, $200, $300] {
    sta $0400 + i, x
}
    inx
    bne clrscr

    ; line drawing, completely unrolled
    ; with assembly pseudos
    lda #$a0

    !for i in range(0, 40) {
        !let y0 = Math.floor(25/40*(i+0.5))
        sta $0400 + y0*40 + i
        sta $0400 + (24-y0)*40 + i
    }
inf: jmp inf  ; halt
}
```

This completely unrolls the line drawing part.  So it's really big at 286 bytes.

Before diving into optimized variants of this code, let's make a couple of observations.

First, we're running on the C64 with the ROM routines banked in.  There's a lot of goodies such as `JSR $E544` to clear the screen.

Second, address calculations on an 8-bit CPU are cumbersome and cost a lot of bytes.  The CPU also doesn't have a multiplier, so computing something like `y*40 + i` usually involves either a bunch of bit shifts or a lookup table, again costing bytes.  To avoid multiplying by 40, we can instead advance the screen pointer incrementally:

```{.c}
    int yslope = (25<<8)/40;
    int yf = yslope/2;
    uint8_t* dst = screenRAM;
    for (int x = 0; x < 40; x++) {
        dst[x] = 0xa0;
        dst[(39-x)] = 0xa0;
        yf += yslope;
        if (yf & 256) { // Carry set?
            dst += 40;
            yf &= 255;
        }
    }
```

We keep adding the line slope to a fixed pointer counter `yf` and when the 8-bit add sets the carry flag, add 40.  (The 8-bit carry flag is "simulated" in C above.)

Here's a complete assembly implementation of this incremental approach.  This is not very size-optimized yet, it produces a hefty 82 byte PRG.

```{.asm}
!include "c64.asm"

+c64::basic_start(entry)

!let screenptr = $20
!let x0 = $40
!let x1 = $41
!let yf = $60

entry: {
        lda #0
        sta x0
        sta $d020
        sta $d021

        ; kernal clear screen
        jsr $e544

        ; set screenptr = $0400
        lda #<$0400
        sta screenptr+0
        lda #>$0400
        sta screenptr+1

        lda #80
        sta yf

        lda #39
        sta x1
xloop:
        lda #$a0
        ldy x0
        ; screenRAM[x] = 0xA0
        sta (screenptr), y
        ldy x1
        ; screenRAM[39-x] = 0xA0
        sta (screenptr), y

        clc
        lda #160  ; line slope
        adc yf
        sta yf
        bcc no_add

        ; advance screen ptr by 40
        clc
        lda screenptr
        adc #40
        sta screenptr
        lda screenptr+1
        adc #0
        sta screenptr+1

no_add:
        inc x0
        dec x1
        bpl xloop

inf:    jmp inf
}
```
