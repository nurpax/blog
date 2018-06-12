---
title: BINTRIS C64 flexible line distance and logo wobble (series part 5)
author: Janne Hellsten
public: false
series: bintris-c64
thumb: /images/bintris/bintris-writer-fld.gif
---

``` {.hakyll-inline-css}
@font-face {
     font-family: "C64 Pro Local";
     src: url("/images/bintris/C64_Pro-STYLE.woff") format("woff");
}

.status {
    font-family: 'C64 Pro Local';
    font-size:0.3em;
}

/* 0.1s duration must match cycle timing in timing-diagram.js */
.move-beam {
  transition: transform 0.1s;
  transition-timing-function: steps(8, start);
}
```

(Looking for the BINTRIS C64 disk image?  Find it [here](/posts/2018-05-21-bintris-on-c64-part-2.html).)

TODO introduce the BINTRIS blog series.

Introduction
------------

This post discusses how BINTRIS uses a technique called the flexible line distance (FLD) to implement full-screen vertical scrolling and suppress Bad Lines when warping the BINTRIS logo.

Here are the effects in action:

<div class="img-columns-2">
    <p class="img-column">
        <img width="100%" class="img-pixelated" src="/images/bintris/bintris-writer-fld.gif" /><em>Vertical scrolling.</em>
    </p>
    <p class="img-column">
        <img width="100%" class="img-pixelated" src="/images/bintris/bintris-logo-wobble.gif" />
        <em>BINTRIS logo warp effect.</em>
    </p>
</div>

Note: timing values in this post are for PAL-B C64 models.

Bad Lines
---------

To understand FLD, you need to grasp the concept of Bad Lines.  I'll summarize Bad Lines here but you can read about it in more detail in the excellent [vic-ii.txt document][fld] (section 3.5).  The same document also explains FLD in section 3.14.2.

The VIC scans out the video frame line-by-line.  Each line takes exactly 63 clock cycles.  On "normal" lines, all these 63 cycles are available to the CPU.  However, on every 8th pixel row (start of a new character row), the VIC needs an extra 40 cycles to fetch one line of character codes from memory.

- TODO: animation with 200 lines + border showing where bad lines are. can be similar to what was done for textmode/bitmap mode.  or maybe this should be side by side with a scanline bad line anim + the whole screen.

<div id="container">
</div>

<script type="text/javascript">

let diagram

$(function () {
    diagram = new diagrams.TimingDiagram()
    diagram.mount(document.getElementById("container"))
})
</script>


Next in series
--------------

TBD

[fld]: http://www.zimmers.net/cbmpics/cbm/c64/vic-ii.txt
[bintris]: http://nurpax.com/bintris
