---
title: BINTRIS C64 game screen rendering (series part 4)
author: Janne Hellsten
public: true
series: bintris-c64
thumb: /images/bintris/gamescreen-social-thumb.png
---

(Looking for the BINTRIS C64 disk image?  Find it [here](/posts/2018-05-21-bintris-on-c64-part-2.html).)

Let's take a look at how the actual BINTRIS game screen is rendered.  This is the first thing I implemented for this projet (after maybe 25 years of C64 dev hiatus) and in hindsight I think it could've been simpler with less sprites and more charset manipulation.

The graphic elements of this screen are:

* The BINTRIS logo at the top (4 multicolor sprites)
* Currently falling bit (one single color sprite)
* Current score (3 multicolor sprites, one sprite per number, max 3 digits)
* The collapse noise animation (3 multicolor sprites)
* The game board rendered in text mode (one character per bit)
* Decimal targets (standard text)

Here's how it looks like (with red & green rectangles highlighting sprite locations):

$bintris_gamescreen_svg$

That's 11 sprites in total.  As the C64 hardware only supports 8 sprites, we need to multiplex hardware sprites with raster interrupts:

1. Program sprites 0-3 for the BINTRIS logo at the top of the frame, before the raster beam reaches the logo.
2. Once the raster beam's past the logo, program sprites 0-6 to be used for the current score (3 sprites), falling bit (1 sprite), and the collapse noise animation (3 sprites).

Note that not all these sprites are always enabled: the noise animation sprites are enabled only when rows are collapsing, the falling bit is turned off during the noise animation, and if the current score is, say, less than 10, we need only one sprite for the score digits.

I did skip over some details, though.  The biggest omission is the BINTRIS logo level up warp effect.  It looks something like this:

<div class="screenshot">
![](/images/bintris/logowarp.gif "BINTRIS logo warp gif"){.img-pixelated width=300 height=80px}
</div>

This effect manipulates the logo sprite X locations on each scanline to achieve the line-by-line distortion effect.  It might not look like much in this .gif capture, but as far as timing sensitive code goes, this is by far the trickiest routine in the game.  There will be a blog about this effect later in the series.

The game board is rendered using text-mode characters.  The charset contains an 8x8 filled rectangle which is used to clear the board area on game start or on level up.  Per-frame rendering only sets the color of each such block.

Board state is represented by two arrays: `setbit` and `bits` (16 bytes each).  The `setbit` array encodes which cells contain a block (one bit per column, one byte per row).  The `bits` array containts the actual bit values.  The code to render the board is pretty simple with these two arrays:

```
piececol:
    .byte DARK_GRAY, BLACK, DARK_GRAY, WHITE

.macro drawboard(colorbuf) {
    .const tsetbits = $62
    .const tbits = $63
    // Note unrolling by 16 is simply due to lazyness, it's not
    // necessary for performance.
    .for (var y = 0; y < 16; y++) {
        lda setbit+y
        sta tsetbits
        lda bits+y
        sta tbits
        ldy bitwidth     // current board bitwidth
xloop:
        lda tbits
        and #1
        lsr tsetbits
        rol
        tax
        lda piececol, x
        lsr tbits
        sta colorbuf+[(y+1)*40], y
        dey
        bne xloop
    }
}
```

That's roughly 31 clock cycles per block.  In the [BINTRIS web version][bintris] each block is a bunch of SVG `<rect>` elements wrapped in a `<g>` element wrapped in a `React.PureComponent`.  I'm pretty sure the cost to render a block in the web version is at least a couple of orders of magnitude higher.  That's not React's fault of course, I should totally get the blame for abusing React for gamedev like this.

Next in series
--------------

The next post will discuss how the main BINTRIS game screen is rendered.

[bintris]: http://nurpax.com/bintris
