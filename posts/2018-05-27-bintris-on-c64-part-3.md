---
title: BINTRIS C64 title screen implementation (series part 3)
author: Janne Hellsten
public: true
series: bintris-c64
---

(Looking for the BINTRIS C64 disk image?  Find it [here](/posts/2018-05-21-bintris-on-c64-part-2.html).)

This blog post discusses the implementation of the BINTRIS C64 title screen.

The title screen consists of a multicolor bitmap at the top and a text mode scroller at the bottom.  Here's how it looks like:

<div class="youtube">
<iframe class="video" src="https://www.youtube.com/embed/akaQcBNG9TE?rel=0&amp;controls=1&amp;showinfo=0" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe></div>

This is a pretty standard text mode scroller: use the horizontal scroll register `$d016` for 0-7 pixel shifting.  When you reach pixel offset 7, reset to pixel offset 0 and move the whole character row left by one character (8 pixels).  Additionally, the scroller text is colored with raster bars.

But wait a minute..  The screen is in multicolor bitmap mode.  How can we use a _text mode_ scroller at the bottom?  Fortunately it's pretty easy to mix bitmap and text mode within a single frame.  Just switch to text-mode right before raster beam reaches the scroller, and switch back to bitmap mode once the raster beam is past it.

The below animation illustrates how the raster IRQs trigger:

$bintris_title_svg$

In summary:

* Line 16 (`irq0`): set multicolor bitmap mode, move character row left by one character if `framecount&7==0`.
* Line 238 (`irq1`): switch to text mode, set horizontal scroll based on `framecount` bits 0-2
* Line 241 (`irq2`): raster bars (using the [double IRQ trick][double-irq] for stable raster), loop back to Line 16

And the same in assembly:

```
.const irq0line = 16
.const textmodeswitchline = 50+200-12
.const rastercolorline = 50+200-9

framecount: .byte 0     // increased at the beginning of each frame

// Macro to save&restore registers and setup the next routine in the
// raster IRQ chain.
.macro irq_start(end_lbl) {
    sta end_lbl-6
    stx end_lbl-4
    sty end_lbl-2
}

.macro irq_end(next, line) {
    :EndIRQ(next, line, false)
    lda #$00
    ldx #$00
    ldy #$00
    rti
}


irq0: {
    irq_start(end)

    inc framecount

    // Set screen mode
    lda #$3b // bitmap mode
    sta $d011
    lda #$18 // multicolor
    sta $d016
    // screen memory ptr
    lda #$18
    sta $d018

    jsr scroller_update_char_row

    irq_end(irq1, textmodeswitchline)
end:
    rti
}

irq1: {
    irq_start(end)

    lda #$1b        // screen on, text mode
    sta $d011

    lda framecount
    and #7
    eor #7 // xor bits 0-2 and leave bit 3 zero for 38 column mode
    sta $d016

    lda #$10 // bank + $0400
    sta $d018

    irq_end(irq2, rastercolorline)
end:
}

// Stable raster IRQ for color bars
irq2: {
    double_irq(end, irq3)

irq3:
    txs

    // Wait exactly 9 * (2+3) cycles so that the raster line is in the border
    ldx #$09
    dex
    bne *-1

    // First line is a bad line so we have only 23 cycles!
    lda colors1+0           // 4 cycles
    sta $d021               // 4 cycles
    .for (var i = 0; i < 6; i++) {
        nop
    }
    bit $fe

    // Next 7 lines are normal lines, so 63 cycles per color change
    ldx #$01
!:
    lda colors1,x           // 4 cycles
    sta $d021               // 4 cycles
    .for (var i = 0; i < (63-15)/2; i++) {
        nop
    }
    inx                     // 2
    cpx #colorend-colors1   // 2
    bne !-                  // 3

    lda #0
    sta $d021

    irq_end(irq0, irq0line)
end:
}
```

I put up a full stand-alone version of this [on github][scroller-asm] -- this should compile on KickAssembler.  It's a slightly cleaned up version of the BINTRIS titlescreen.  For some historical reason I used VIC bank 1 instead of the default and made screen RAM reside at `$4400` instead of `$0400`.  This can make the source a bit harder to follow.

Next in series
--------------

The next post will discuss how the main BINTRIS game screen is rendered.

[scroller-asm]: https://github.com/nurpax/c64-samples/tree/master/text-scroller
[double-irq]: http://codebase64.org/doku.php?id=base:double_irq_explained
