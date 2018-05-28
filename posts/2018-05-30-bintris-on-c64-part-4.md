---
title: BINTRIS C64 game screen rendering (series part 4)
author: Janne Hellsten
public: true
series: bintris-c64
---

(Looking for the BINTRIS C64 disk image?  Find it [here](/posts/2018-05-21-bintris-on-c64-part-2.html).)

Let's take a look at how the actual BINTRIS game screen is rendered.  This is the first thing I implemented for this projet (after maybe 25 years of C64 dev hiatus) and in hindsight I think it could've been simpler with less sprites and more charset manipulation.

The graphical elements of this screen are:

* The BINTRIS logo at the top (4 multicolor sprites)
* The game board rendered in text mode (one character per bit)
* Currently falling bit (one single color sprite)
* Current score (3 multicolor sprites, one sprite per number, max 3 digits)
* The collapse noise animation (3 multicolor sprites)
* Decimal targets (standard text)

Here's how it looks like (with red & green rectangles overlayed on top of sprites):

$bintris_gamescreen_svg$

You may have noticed there's a total of 11 sprites.  As the C64 hardware only supports 8 sprites, we need to multiplex hardware sprites with raster interrupts:

1. Program sprites 0-3 for the BINTRIS logo at the top of the frame, before the raster beam reaches the logo.
2. Once the raster beam's past the BINTRIS logo, program sprites 0-6 to be used for the current score (3 sprites), falling bit (1 sprite), and the collapse noise animation (3 sprites).

Note that not all these sprites are always enabled: the noise animation sprites are enabled only when rows are collapsing, the falling bit is turned off during the noise animation, and if the current score is, say, less than 10, we need only one sprite for the score digits.

Having said all of the above, I realize some of this might've been better implemented with just text rendering using a modified charset.  But sprites got the job done and I left it at that.


```
.macro drawboard(screen, colorbuf, piececol) {
    .const tsetbits = $62
    .const tbits = $63
    .const col = $64
...
```

TODO bintris logo wobble anim

Next in series
--------------

The next post will discuss how the main BINTRIS game screen is rendered.

[scroller-asm]: https://github.com/nurpax/c64-samples/tree/master/text-scroller
