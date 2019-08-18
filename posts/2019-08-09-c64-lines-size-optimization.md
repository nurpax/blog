---
title: Dirty tricks 6502 programmers use
author: Janne Hellsten
public: true
syntax-css: syntax2.css  # use a newer syntax CSS for this file
thumb: /images/c64/lines/lines-scroll.gif
---
``` {.hakyll-inline-css}
.narrowlines {
    line-height: 0.9em;
}
.overflow {
	width: 100%;
	overflow-x: auto;
}
.center {
	display: flex;
	flex-direction: column;
	align-items: center;
}
```

This post recaps some of the C64 coding tricks used in my little [Commodore 64 coding competition](https://twitter.com/nurpax/status/1159192477598965766).  The competition rules were simple: make a C64 executable (PRG) that draws two lines to form the below image.  The objective was to do this in as few bytes as possible.

<div class="center">
<img width="75%" class="img-pixelated" src="/images/c64/lines/lines-2x.png" />
</div>

Entries were posted as Twitter replies and DMs, containing only the PRG byte-length and an MD5 hash of the PRG file.

Here's a list of participants with source code links to their submissions:

- [Philip Heron](https://twitter.com/fsphil) ([code](https://github.com/fsphil/tinyx) - 34 bytes - compo winner)
- [Geir Straume](https://twitter.com/GeirSigmund) ([code](https://c64prg.appspot.com/downloads/lines34b.zip) - 34 bytes)
- [Petri Häkkinen](https://twitter.com/petrih3) ([code](https://github.com/petrihakkinen/c64-lines) - 37 bytes)
- [Mathlev Raxenblatz](https://twitter.com/laubzega) ([code](https://gist.github.com/laubzega/fb59ee6a3d482feb509dae7b77e925cf) - 38 bytes)
- [Jan Achrenius](https://twitter.com/achrenico) ([code](https://twitter.com/achrenico/status/1161383381835362305) - 48 bytes)
- [Jamie Fuller](https://twitter.com/jamie30dbs) ([code](https://github.com/30dbs/c64x) - 50 bytes)
- [David A. Gershman](https://twitter.com/dagershman) ([code](http://c64.dagertech.net/cgi-bin/cgiwrap/c64/index.cgi?p=xchallenge/.git;a=tree) - 53 bytes)
- [Janne Hellsten](https://twitter.com/nurpax) ([code](https://gist.github.com/nurpax/d429be441c7a9f4a6ceffbddc35a0003) - 56 bytes)

(If I missed someone, please let me know and I'll update the post.)

The rest of this post focuses on some of the assembly coding tricks used in the compo submissions.

## Basics

The C64 default graphics mode is the 40x25 charset mode.  The framebuffer is split into two arrays in RAM:

* `$0400` (Screen RAM, 40x25 bytes)
* `$d800` (Color RAM, 40x25 bytes)

To set a character, you store a byte into screen RAM at `$0400` (e.g., `$0400+y*40+x`).  Color RAM is by default initialized to light blue (color 14) which happens to be the same color we use for the lines -- meaning we can leave color RAM untouched.

You can control the border and background colors with memory mapped I/O registers at `$d020` (border) and `$d021` (background).

Drawing the two lines is pretty easy as we can hardcode for the fixed line slope.  Here's a C implementation that draws the lines and dumps screen contents on stdout (register writes stubbed out and screen RAM is `malloc()`'d to make it run on PC):

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

The screen codes used above are: `$20` (blank) and `$a0` (8x8 filled block).  If you run it, you should see ASCII art for the two lines:

```{.narrowlines}
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

    !for i in range(40) {
        !let y0 = Math.floor(25/40*(i+0.5))
        sta $0400 + y0*40 + i
        sta $0400 + (24-y0)*40 + i
    }
inf: jmp inf  ; halt
}
```

This completely unrolls the line drawing part resulting in a fairly large 286 byte PRG.

Before diving into optimized variants, let's make a couple of observations:

First, we're running on the C64 with the ROM routines banked in.  There's a bunch of subroutines in ROM that may be useful for our little program.  For example, you can clear the screen with `JSR $E544`.

Second, address calculations on an 8-bit CPU like the 6502 can be cumbersome and cost a lot of bytes.  This CPU also doesn't have a multiplier, so computing something like `y*40+i` usually involves either a bunch of logical shifts or a lookup table, again costing bytes.  To avoid multiplying by 40, we can instead advance the screen pointer incrementally:

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

We keep adding the line slope to a fixed point counter `yf` and when the 8-bit addition sets the carry flag, add 40.

Here's the incremental approach implemented in assembly:

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

At 82 bytes, this is still pretty hefty.  A couple of obvious size problems arise from 16-bit address computations:

Setting up the `screenptr` value for indirect-indexed addressing:

```{.asm}
        ; set screenptr = $0400
        lda #<$0400
        sta screenptr+0
        lda #>$0400
        sta screenptr+1
```

Advancing `screenptr` to the next row by adding 40:
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

Sure this code could probably be made smaller but what if we didn't need manipulate 16-bit addresses in the first place?  Let's see this can be avoided.

## Trick 1: Scrolling!

Instead of plotting the line across the screen RAM, we draw only on the last Y=24 screen row, and scroll the whole screen up by calling a "scroll up" ROM function with `JSR $E8EA`!

The x-loop becomes:

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
        sta $0400 + 24*40, x

        adc yf
        sta yf
        bcc no_scroll
        ; scroll screen up!
        jsr $e8ea
no_scroll:
        inc x0
        dec x1
        bpl xloop
```

Here's how the line renderer progresses with this trick:

<div class="center">
<img width="75%" class="img-pixelated" src="/images/c64/lines/lines-scroll.gif" />
</div>

This trick was one of my favorites in this compo.  It was also independently discovered by pretty much every participant.

## Trick 2: Self-modifying code

The code to store the pixel values ends up being roughly:

```{.asm}
        ldx x1
        ; hardcoded absolute address to last screen line
        sta $0400 + 24*40, x
        ldx x0
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
        sta $0400 + 24*40, x
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

Making wild assumptions about the running environment was considered OK in this compo: the line drawing PRG is the first thing that's run after C64 power on, and there was no requirement to exit cleanly back to the BASIC prompt.  So anything you find from the initial environment upon entry to your PRG, you can and should use to your advantage.  Here are some of the things that were considered "constant" upon entry to the PRG:

- A, X, Y registers were assumed to be all zeros
- All CPU flags cleared
- Zeropage (addresses `$00`-`$ff`) contents

Similarly, if you called any KERNAL ROM routines, you could totally take advantage of any side-effects they might have: returned CPU flags, temporary values set into zeropage, etc.

After the first few size-optimization passes, everyone turned their eyes on this machine monitor view to look for any interesting values:

<div class="center overflow">
<img class="img-pixelated" src="/images/c64/lines/monitor-screenshot.png" />
</div>

The zeropage indeed contains some useful values for our purposes:

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

Philip's [winning entry](https://github.com/fsphil/tinyx/blob/master/x34/x34.s) takes this to the extreme.  Recall the address of the last char row `$07C0` (==`$0400+24*40`).  This value does not exist in the zeropage on init.  However, as a side-effect of how the ROM "scroll up" subroutine uses zeropage temporaries, addresses `$D1-$D2` will contain `$07C0` on return from this function.  So instead of `STA $07C0,x` to store a pixel, you can use the one byte shorter indirect-indexed addressing mode store `STA ($D1),y`.

## Trick 4: Smaller startup

A typical C64 PRG binary file contains the following:

- First 2 bytes: loading address (usually `$0801`)
- 12 bytes of BASIC startup sequence

The BASIC startup sequence looks like this (addresses `$801-$80C`):

```
0801: 0B 08 0A 00 9E 32 30 36 31 00 00 00
080D: 8D 20 D0     STA $D020
```

Without going into details about [tokenized BASIC memory layout](https://www.c64-wiki.com/wiki/BASIC_token), this sequence more or less amounts to "10 SYS 2061".  Address `2061` (`$080D`) is where our actual machine code program starts when the BASIC interpreter executes the SYS command.

14 bytes just to get going feels excessive.  Philip, Mathlev and Geir had used some clever tricks to get rid of the BASIC sequence altogether.  This requires that the PRG is loaded with `LOAD "*",8,1` as `LOAD "*",8` ignores the PRG loading address (the first two bytes) and always loads to `$0801`.

<div class="center">
<img width="75%" class="img-pixelated" src="/images/c64/lines/vice-screen-sys.png" />
</div>

Two methods were used:

- The stack trick
- The BASIC warm reset vector trick

### The stack trick

The trick is to stomp the CPU stack at `$01F8` with a value that points to our desired entry point.  This is done by crafting a PRG that starts with a 16-bit pointer pointing to our code and loading the PRG into `$01F8`:

```{.asm}
    * = $01F8
    !word scroll - 1  ; overwrite stack

scroll:	jsr $E8EA
```

Once the BASIC loader (see [disassembly](https://www.pagetable.com/c64disasm/#F4A5)) has finished loading and returns to its caller with `RTS`, instead of returning to whoever called LOAD, it returns right into our PRG.

### The BASIC warm reset vector trick

This is a little easier to explain by just looking at the PRG disassembly.

```
02E6: 20 EA E8    JSR $E8EA
02E9: A4 D5       LDY $D5
02EB: A9 A0       LDA #$A0
02ED: 99 20 D0    STA $D020,Y
02F0: 91 D1       STA ($D1),Y
02F2: 9D B5 07    STA $07B5,X
02F5: E6 D6       INC $D6
02F7: 65 90       ADC $90
02F9: 85 90       STA $90
02FB: C6 D5       DEC $D5
02FD: 30 FE       BMI $02FD
02FF: 90 E7       BCC $02E8
0301: 4C E6 02    JMP $02E6
```

Notice the last line (`JMP $02E6`).  The JMP instruction starts at address `$0301` with the branch target stored in addresses `$0302-$0303`.

When this code is loaded into memory starting at address `$02E6`, a value of `$02E6` is written to addresses `$0302-$0303`.  Well, location `$0302-$0303` has a special meaning: it contains a pointer to the "BASIC idle loop" (see [C64 memory map](http://sta.c64.org/cbm64mem.html) for details).  Loading the PRG overwrote this location with `$02E6` and so when the BASIC interpreter tries to jump to the idle loop after warm reset, it never enters the idle loop but instead ends up in the line renderer!

### Other BASIC startup related tricks

Petri had discovered [another BASIC start trick](https://github.com/petrihakkinen/c64-lines/blob/master/main37.asm) which allows injecting your own constants into the zeropage.  In this method, you hand-craft your own tokenized BASIC start sequence and encode your constants into the BASIC program line number.  The BASIC line number, ahem, your constants, will be stored in addresses `$39-$3A` upon entry.   Very clever!

## Trick 5: Unconventional control flow

Here's a somewhat simplified version of the x-loop that draws only a single line and then halts execution once the line is done:

```{.asm}
        lda #39
        sta x1
xloop:
        lda #$a0
        ldx x1
        sta $0400 + 24*40, x

        adc yf
        sta yf
        bcc no_scroll
        ; scroll screen up!
        jsr $e8ea
no_scroll:
        dec x1
        bpl xloop

        ; intentionally halt at the end
inf:    jmp inf
```

This has a bug in it, though.  When we've drawn the last pixel of a line, we should NOT scroll the screen up anymore.  Thus we need more branching to skip scrolling on the last pixel write:

```{.asm}
        lda #39
        sta x1
xloop:
        lda #$a0
        ldx x1
        sta $0400 + 24*40, x

        dec x1
        ; skip scrolling if last pixel
        bmi done

        adc yf
        sta yf
        bcc no_scroll
        ; scroll screen up!
        jsr $e8ea
no_scroll:
        jmp xloop
done:

        ; intentionally halt at the end
inf:    jmp inf
```

The control flow looks a lot like what a C compiler would output from a structured program.  The code to skip the last scroll introduced a new `JMP abs` instruction that takes up 3 bytes.  Conditional branches are only two bytes as they encode the branch target using a relative 8-bit immediate.

The "skip last scroll" JMP can be avoided by moving the scroll up call to the top of the loop, and restructuring the control flow a bit.  This is the pattern Philip had come up with:

```{.asm}
        lda #39
        sta x1
scroll: jsr $e8ea
xloop:
        lda #$a0
        ldx x1
        sta $0400 + 24*40, x

        adc yf
        sta yf
        dec x1     ; doesn't set carry!
inf:    bmi inf    ; hang here if last pixel!
        bcc xloop  ; next pixel if no scroll
        bcs scroll ; scroll up and continue
```

This completely eliminates one 3 byte JMP and converts another JMP to a 2 byte conditional branch, saving 4 bytes in total.

## Trick 6: Bitpacked line drawing

Some of the entries didn't use a line slope counter but rather they had bit-packed the line pattern into an 8-bit constant.  This packing comes out of a realisation that the pixel position along the line follows a repeating 8 pixel pattern:

```{.c}
int mask = 0xB6; // 10110110
uint8_t* dst = screenRAM;
for (int x = 0; x < 40; x++) {
    dst[x] = 0xA0;
    if (mask & (1 << (x&7))) {
        dst += 40; // go down a row
    }
}
```

This is translates to pretty compact assembly.  The slope counter variants tended to be even smaller, though.

## Winner entry

This is the [winning 34 byte entry](https://github.com/fsphil/tinyx/blob/master/x34/x34.s) from Philip.  Most of the above really comes together nicely in his code:

```{.asm}
ov = $22 ; == $40, initial value for the overflow counter
ct = $D5 ; == $27 / 39, number of passes. Decrementing, finished at -1
lp = $D1 ; == $07C0, pointer to bottom line. Set by the kernal scroller

        ; Overwrite the return address of the kernal loader on the stack
        ; with a pointer to our own code

        * = $01F8
        .word scroll - 1

scroll: jsr $E8EA    ; Kernal scroll up, also sets lp pointer to $07C0
loop:   ldy ct	     ; Load the decrementing counter into Y (39 > -1)
        lda #$A0     ; Load the PETSCII block / black col / ov step value
        sta $D020, y ; On the last two passes, sets the background black
p1:     sta $07C0    ; Draw first block (left > right line)
        sta (lp), y  ; Draw second block (right > left line)
        inc p1 + 1   ; Increment pointer for the left > right line
        adc ov	     ; Add step value $A0 to ov
        sta ov
        dec ct	     ; Decrement the Y counter
        bmi *	     ; If it goes negative, we're finished
        bcc loop     ; Repeat. If ov didn't overflow, don't scroll
        bcs scroll   ; Repeat. If ov overflowed, scroll
```

## Why stop at 34 bytes, though?

Once the competition was over, everyone shared code and notes, and a number of lively conversations took place on how to do even better.  Several smaller variants were posted after the deadline:

- [Philip - 33 bytes](https://gist.github.com/fsphil/05deaa06804b9b2054260b616cafed4b)
- [Philip - 32 bytes](https://gist.github.com/fsphil/01bda1a9dd58c219002ddd6e18b36c3f)
- [Petri - 31 bytes](https://github.com/petrihakkinen/c64-lines/blob/master/main31.asm)
- [Philip - 29 bytes](https://gist.github.com/fsphil/7655a394ec5f953c910e9d9369dced56)

You should check them out -- there are some real gems to be found.

...

Thanks for reading.  And most of all, thanks Mathlev, Phil, Geir, Petri, Jamie, Jan and David for your participation.  (I hope I didn't miss anyone -- it was really difficult to keep track of these in Twitter mentions!)

PS. Petri had named my compo "@nurpax's annual C64 size optimization compo", so uhm, see you next year, I guess.
