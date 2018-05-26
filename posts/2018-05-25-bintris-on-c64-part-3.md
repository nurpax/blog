---
title: BINTRIS title screen implementation (series part 3)
author: Janne Hellsten
public: true
series: bintris-c64
---

(Looking for the BINTRIS disk image?  Find it [here](/posts/2018-05-21-bintris-on-c64-part-2.html).)

This blog post discusses the implementation of the BINTRIS title screen.

The title screen consists of a multicolor bitmap at the top and a text mode scroller at the bottom.  Here's how it looks like:

<div class="youtube">
<iframe class="video" src="https://www.youtube.com/embed/akaQcBNG9TE?rel=0&amp;controls=1&amp;showinfo=0" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe></div>

The scroller is a pretty standard text mode scroller: use the horizontal scroll register `$d016` for 0-7 pixel shifting, when you reach pixel offset 7, reset to pixel offset 0 and move the whole character row left.  These types of text mode scrollers are fast and easy to implement.

Here comes the trick though: the upper part of the screen is in multicolor bitmap mode.  How can we use a text mode scroller at the bottom?  Easy!  Just switch to text-mode right before the scroller, and switch back to bitmap mode once the raster beam is past the text scroller.

I made this fancy SVG animation to highlight the raster IRQ locations:

$bintris_title_svg$

Here's the source code to the relevant IRQ routine parts:

```
.const irq0line = 16
.const textmodeswitcahline = 50+200-12
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

    // Set screen mode
    lda #$3b    // bitmap mode
    sta $d011
    lda #$18    // multicolor
    sta $d016
    lda #$18    // screen memory ptr
    sta $d018

    inc framecount
    jsr PLAY_MUSIC

    // Move scroller character row contents & add new text
    // if (framecount&7)==0
    jsr scroller_chars_irq_update

    irq_end(irq1, textmodeswitchline)
end:
}

irq1: {
    irq_start(end)

    // screen on, text mode
    lda #$1b
    sta $d011

    lda framecount
    and #7
    eor #7 // invert bits 0-2 and leave bit 3 zero for 38 column mode
    sta $d016

    lda #$10 // screen memory ptr = bank + $0400
    sta $d018

    irq_end(irq2, rastercolorline)
end:
}

irq2: {
    double_irq(end, irq3)
irq3:
    txs

    // wait 45 cycles so that the raster beam is in the border
    ldx #$09
    dex
    bne *-1

    // first line (bad line so have only 23 cycles!)
    lda colors1+0           // 4 cycles
    sta $d021               // 4 cycles
    .for (var i = 0; i < 6; i++) {
        nop
    }
    bit $fe

    // Next 7 lines are normal lines, so 63 cycles per color change
    ldx #$01
    // the loop total must be 63 cycles
!:
    lda colors1,x           // 4 cycles
    sta $d021               // 4 cycles
    .for (var i = 0; i < (63-15)/2; i++) {
        nop
    }
    inx                     // 2
    cpx #colorend-colors1   // 2
    bne !-                  // 3

    irq_end(irq0, irq0line)
end:
}
```

A bunch of details have been omitted for brevity.  Let me know if you're interested in the full source code for this, I might rip it out of BINTRIS and make a standalone .asm file out of it.

XXX need standalone source, maybe?

Next in series
--------------

Next topic TBD.

[bintris]: http://nurpax.com/bintris
