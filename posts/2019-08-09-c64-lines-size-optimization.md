---
title: Dirty tricks 6502 programmers use
author: Janne Hellsten
public: true
syntax-css: syntax2.css  # use a newer syntax CSS for this file
thumb: /images/c64/lines/lines-scroll.gif
---

- TODO twitter compo and instructions: https://gist.github.com/nurpax/96285c08710387e73ea5e039e27980af
- TODO list participants with names (twitter handles too?) with links to source code.

The competition rules were simple: Make a C64 executable (PRG) that draws two lines to form the below image.  The objective was to do this in as few bytes as possible.

<img width="75%" class="img-pixelated" src="/images/c64/lines/lines-2x.png" />

Entries were posted as Twitter replies and DMs, containing only the PRG byte-length and an MD5 hash of the PRG file.

This blog post is a review of the sort of assembly coding tricks that were used in compo submissions.

## Basics

The C64 default graphics mode is the 40x25 charset mode.  The framebuffer split into two arrays in RAM:

* `$0400` (screen RAM, 40x25 bytes)
* `$d800` (color RAM, 40x25 bytes)

To set a character, you store a byte into into the `$0400` (e.g., `$0400 + y*40 + x`) buffer.  The color RAM is by default initialized to light blue (color 14) which happens to be the same color we use for the lines, so we don't have to touch the color RAM in this exercise.

You can control the border and background colors with memory mapped I/O registers at `$d020` (border) and `$d021` (background).

Drawing the two lines is pretty easy as we can hardcode for the fixed line slope.  Here's a C implementation that draws the lines and dumps screen contents on stdout (register writes stubbed out and screen RAM is `malloc()`'d memory that it runs on PC):

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

Before diving into optimized variants of this code, let's make a couple of observations:

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

Here's a complete assembly implementation of the incremental approach:

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

At 82 bytes, this is still pretty hefty.  A couple of obvious problems in this code come from 16-bit address computations.

Setting up the `screenptr` value for indirect-indexed addressing:

```{.asm}
        ; set screenptr = $0400
        lda #<$0400
        sta screenptr+0
        lda #>$0400
        sta screenptr+1
```

Advancing `screenptr` to the next line by adding 40 to it.
```{.asm}
        ; advance screen ptr by 40
        clc
        lda screenptr
        adc #40
        sta screenptr
        lda screenptr+1
        adc #0
        sta screenptr+1
```

Sure this code could probably be more compact but what if we didn't need manipulate 16-bit addresses in the first place?  Turns out we can do just that!

## Trick 1: Scrolling!

Instead of plotting the line across the screen RAM, we can only ever draw on the last Y=24 screen row, and scroll the whole screen up by calling a "scroll up" ROM function with `JSR $E8EA`!

With this, the X-loop becomes something like below:

```{.asm}
        lda #0
        sta x0
        lda #39
        sta x1
xloop:
        lda #$a0
        ldx x0
        ; hardcoded absolute address to last screen line
        sta $0400 + 24*40, x
        ldx x1
        ; hardcoded absolute address to last screen line
        sta $0400 + 24*40, x

        adc yf
        sta yf
        bcc no_scroll
        ; scroll screen up!
        jsr $e8ea
no_scroll:
        inc x1
        dec x0
        bpl xloop
```

Here's how the line renderer progresses with this trick:

<img width="75%" class="img-pixelated" src="/images/c64/lines/lines-scroll.gif" />

This trick was one of my favorites in this compo.  It was also independently discovered by pretty much all the participants.

## Trick 2: Self-modifying code

The code to store the pixel values ends up being roughly:

```{.asm}
        ldx x1
        ; hardcoded absolute address to last screen line
        sta $0400 + 24*40, x
        ldx x0
        ; hardcoded absolute address to last screen line
        sta $0400 + 24*40, x
        inc x0
        dec x1
```

This encodes into the following 14 byte sequence:

```
0803: A6 22               LDX $22
0805: 9D C0 07            STA $07C0,X
0808: A6 20               LDX $20
080A: 9D C0 07            STA $07C0,X
080D: E6 22               INC $22
080F: C6 20               DEC $20
```

There's a more compact way to write this using self-modifying code (SMC)..

```{.asm}
        ldx x1
        ; hardcoded absolute address to last screen line
        sta $0400 + 24*40, x
        ; hardcoded absolute address to last screen line
addr0:  sta $0400 + 24*40
        ; advance the second x-coord with SMC
        inc addr0+1
        dec x1
```

..which encodes to 13 bytes:

```
0803: A6 22               LDX $22
0805: 9D C0 07            STA $07C0,X
0808: 8D C0 07            STA $07C0
080B: EE 09 08            INC $0809
080E: C6 22               DEC $22
```

## Trick 3: Exploiting the power on state

It was considered OK to make wild assumptions about the running environment: the line drawing PRG is the first thing that runs after C64 power on, and there was no requirement to exit cleanly back to the BASIC prompt.  So anything you find from the initial environment upon entry to your PRG, you can and should use to your advantage.  Here are some of the things that were considered "constant" upon entry to the PRG:

- A, X, Y registers were assumed to be all zeros
- All CPU flags cleared
- Zeropage (addresses `$00`-`$ff`) contents

Similarly, if you called any KERNAL ROM routines, you could totally take advantage of any side-effects they might have: returned CPU flags, temporary values set into zeropage, etc.

After the first N passes of size-optimization, everyone turned their eyes on this machine monitor view to look for any interesting constants:

<img class="img-pixelated" src="/images/c64/lines/monitor-screenshot.png" />

Indeed, the zeropage did contain some useful values:

- `$d5`: 39/$27 == line length - 1
- `$22`: 64/$40 == initial value for line slope counter

You can use these to shave off a few bytes at init time.  For example:

```{.asm}
!let x0 = $20
        lda #39      ; 0801: A9 27    LDA #$27
        sta x0       ; 0803: 85 20    STA $20
xloop:
        dec x0       ; 0805: C6 20    DEC $20
        bpl xloop    ; 0807: 10 FC    BPL $0805
```

As `$d5` contains a value 39, you can map your `x0` counter to point to `$d5` and skip the LDA/STA pair:

```{.asm}
!let x0 = $d5
        ; nothing here!
xloop:
        dec x0       ; 0801: C6 D5    DEC $D5
        bpl xloop    ; 0803: 10 FC    BPL $0801
```

Philip's [winning entry](https://github.com/fsphil/tinyx/blob/master/x34/x34.s) takes this to the next level.  Recall the address of the last char row `$07C0` (==`$0400+24*40`).  This value does not exist in the zeropage on init.  However, as a side-effect of how the ROM `JSR $E8EA` uses zeropage temporaries, addresses `$D1-$D2` will contain `$07C0` on exit from the scroll up function.  So instead of `STA $07C0,x` to store a pixel, you can use the indirect-indexed addressing mode and write `STA ($D1),y` to save a byte.

Philip went even further with his `$E8EA` exploits, you should read through his 32 byte work of art [here](https://github.com/fsphil/tinyx/blob/master/x32/x32.s).

## Trick 4: BASIC startup tricks

A typical C64 PRG binary file contains the following:

- First 2 bytes: loading address (usually `$0801`)
- 12 bytes of BASIC startup sequence

The BASIC startup sequence looks like this (addresses `$801-$80C`):

```
0801: 0B 08 0A 00 9E 32 30 36
0809: 31 00 00 00
080D: 8D 20 D0                    STA $D020
```

Without going into details about [tokenized BASIC memory layout](https://www.c64-wiki.com/wiki/BASIC_token), this sequence more or less amounts to "10 SYS 2061".  Address `2061` (`$080D`) is where our actual machine code program starts when the BASIC interpreter executes the SYS command.

Well, 14 bytes just to get going feels excessive.  Philip and Geir had used some clever tricks to get rid of the BASIC sequence altogether.  This requires that the PRG is loaded with `LOAD "*",8,1` as `LOAD "*",8` ignores the PRG loading address (the first two bytes) and always loads to `$0801`.

<img width="75%" class="img-pixelated" src="/images/c64/lines/vice-screen-sys.png" />

Two methods were used:

- The stack trick
- The BASIC warm reset vector trick

### The stack trick

- TODO this basically causes the first RTS in the BASIC interpreter to return to our code.

```{.asm}
	* = $01F8
	!word scroll - 1

scroll:	jsr $E8EA
```

### The BASIC warm reset vector trick

This is a little easier to explain by just looking at the PRG disassembly.

```
02E6: 20 EA E8                    JSR $E8EA
02E9: A4 D5                       LDY $D5
02EB: A9 A0                       LDA #$A0
02ED: 99 20 D0                    STA $D020,Y
02F0: 91 D1                       STA ($D1),Y
02F2: 9D B5 07                    STA $07B5,X
02F5: E6 D6                       INC $D6
02F7: 65 90                       ADC $90
02F9: 85 90                       STA $90
02FB: C6 D5                       DEC $D5
02FD: 30 FE                       BMI $02FD
02FF: 90 E7                       BCC $02E8
0301: 4C E6 02                    JMP $02E6
```

Notice the last line (`JMP $02E6`).  The JMP instruction starts at address `$0301` with the branch target stored in addresses `$0302-$0303`.

When this code is loaded into memory starting at address `$02E6`, a value of `$02E6` is written to addresses `$0302-$0303`.  Well, location `$0302-$0303` has a special meaning: it contains a pointer to the "BASIC idle loop" (see [C64 memory map](http://sta.c64.org/cbm64mem.html) for details).  Loading the PRG overwrote this location with `$02E6` and so when the BASIC interpreter tries to jump to the idle loop after warm reset, it never enters the idle loop but instead ends up in the line renderer!

### Other BASIC startup related tricks

Petri had discovered [another BASIC start trick](https://github.com/petrihakkinen/c64-lines/blob/master/main37.asm) which allows injecting your own constants into the zeropage.  In this method, you hand-craft your own tokenized BASIC start sequence and encode your constants into the BASIC program line number.  The BASIC line number, ahem, your constants, will be stored in addresses `$39-$3A` upon entry.   Very clever!

## Winner entry

TODO source and disassembly of the winner submission

## Post-deadline 32 byte version

TODO source and disassembly of the 32 byte version that uses pretty much every trick explained in this post.

## Closing

Thanks for reading.
