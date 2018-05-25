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

The scroller is a pretty standard text mode scroller: use the horizontal scroll register `$d016` for 0-7 pixel shifting, when you reach pixel offset 7, reset to pixel offset 0 and move the whole character row left.

This type of a text mode scroller is easy to implement and plenty fast.

Here comes the trick though: the upper part of the screen is in multicolor bitmap mode.  How can we use a text mode scroller at the bottom?  Easy!  Just switch to text-mode right before the scroller, and switch back to bitmap mode once the raster beam is past the text scroller.

$bintris_title_svg$

Next in series
--------------

Next topic TBD.

[bintris]: http://nurpax.com/bintris
