---
title: BINTRIS on the C64 (series part 1)
author: Janne Hellsten
date: May 18, 2018
public: true
series: bintris-c64
---

Introduction
------------

This is the first part of a blog series on making a small game called [BINTRIS][bintris] for the Commodore 64.

Here are a some of the topics I plan to cover in the series:

* Tooling
* Sprite multiplexing
* Mixed text and bitmap mode display
* Vertical raster scroll and flexible line distance (FLD)
* A line-by-line sprite sine-wave animation
* SID sound effects

As most of these are well-documented on the internet, my posts won't be full-blown tutorials but rather examples of how to apply the above techniques in a game.  I'll start this series by covering tools.

BINTRIS
-------

BINTRIS is a puzzle game that plays a little like Tetris but using binary numbers.  Here's a little gameplay video:

<div class="youtube">
<iframe class="video" src="https://www.youtube.com/embed/RSAlFunPlYI?rel=0&amp;controls=0&amp;showinfo=0" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>
</div>

The idea is that each row is a binary number (black is zero, white is one) and you need to build rows that match the numbers listed below the DECIMAL heading.

Here's a little teaser showcasing the readme.prg file from the bintris disk image:

<div class="youtube">
<iframe class="video" src="https://www.youtube.com/embed/AYDKdRmlxFs?rel=0&amp;controls=0&amp;showinfo=0" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>
</div>

The game will be released any day now -- I'll update this post with a download link when it's out.  I'll also post some more videos once I have the final version captured on real hardware.

How to develop for the C64
--------------------------

Probably the best choice for developing for the C64 is to use an emulator.  I went with [VICE](vice).

I used [KickAssembler](kickass) for compiling my assembly code.  Its macro support is some seriously good stuff -- I wish I had something like this when I used to work on PC demos.

For editing and compiling 6510 assembly source, I ended up going with the [Sublime Text 3 and KickAssembler package](https://packagecontrol.io/packages/Kick%20Assembler%20(C64)).  This sets up your editor such that you can just press F7 and it will compile and run your code on VICE.  It also supports building a debug version of your binary that allows setting breakpoints to break into the VICE monitor (read: debugger).  The monitor can be remotely accessed with telnet from your host machine.  Editing monitor commands on your host makes things like copy & paste easier.

I had some problems with the VICE emulator though.  For example, sound is pretty buggy on my MacBook Pro and sometimes buggy even on Windows 10.  I also couldn't get video capture to work on Mac at all, and on Windows h.264 export crashes the emulator right away.  I found [hoxs64](http://www.hoxs64.net/default.aspx) to have more stable sound but it's otherwise not as featureful as VICE.  I also tried to build my own VICE binary on Windows but that got ugly real quick when I tried to deal with the MinGW build environment.

Other tools used to develop BINTRIS:

* [Multipaint](multipaint) -- for painting pixel graphics and sprites.  (Although in the end, most sprites were done in Photoshop.)
* [PETSCII editor](petscii) -- for PETSCII "art" used in the readme.prg.
* [SID sound editor](https://github.com/nurpax/c64-sid-edit) -- I wrote this C64 sound editor to make it easier to tweak SID registers for my sound effects.
* [VICE snapshot parser](https://github.com/nurpax/c64-sid-edit/blob/master/GrabSounds.hs) -- a Haskell program I wrote for parsing memory snapshots.  I used this to save/load the SID editor sounds.  Rather than implementing save/load into my C64 app, I grabbed the sound data directly from RAM.
* [GoatTracker](https://sourceforge.net/projects/goattracker2/) - for SID music.
* Python - a bunch of LUT generators using numpy and PIL.

C64 learning resources
----------------------

Here are some of my favorite C64 resources:

* [The MOS 6567/6569 video controller (VIC-II) and its application in the Commodore 64](http://www.zimmers.net/cbmpics/cbm/c64/vic-ii.txt)
 -- a pretty exchaustive document on the VIC-II graphics chip.
* [c64-wiki](https://www.c64-wiki.com/wiki/Main_Page) -- I often use this for looking up assembly instructions (see e.g. [BPL](https://www.c64-wiki.com/wiki/BPL).  I'm not a huge fan of some of the pages though.  For example, the instruction reference pages would be better if they specified how an instruction works with pseudo code.
* [C64 memory map](http://sta.c64.org/cbm64mem.html) -- this is a super helpful reference for C64 registers.
* [codebase64](http://codebase64.org/doku.php) -- found a bunch of good examples here (but also many bugs!)

The list is missing a good starter guide for 6510 assembly.

Next in series
--------------

The next blog in the series will showcase the game itself, hopefully with final version captured on real hardware.

<!--$snippet("includes/bintris-c64-series.html")$-->

<!--
Bug:

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">You can tell the release day is near when your game goes all wonky like this. <a href="https://twitter.com/hashtag/bintris?src=hash&amp;ref_src=twsrc%5Etfw">#bintris</a> <a href="https://twitter.com/hashtag/c64?src=hash&amp;ref_src=twsrc%5Etfw">#c64</a> <a href="https://t.co/Dr7orgnkSF">pic.twitter.com/Dr7orgnkSF</a></p>&mdash; Janne Hellsten (@nurpax) <a href="https://twitter.com/nurpax/status/993934121935228929?ref_src=twsrc%5Etfw">May 8, 2018</a></blockquote>
<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
-->

<!--
<div class="youtube">
<iframe class="video" src="https://www.youtube.com/embed/8UjTcipfRJc?rel=0" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>
</div>
-->

Thoughts?  Questions?  Let's discuss on [/r/c64coding](https://www.reddit.com/r/c64coding).

[bintris]: http://nurpax.com/bintris
[css3stars]: https://codepen.io/keithclark/pen/ibEnk
[animated]: https://facebook.github.io/react-native/docs/animated.html
[vice]: http://vice-emu.sourceforge.net/
[kickass]: http://www.theweb.dk/KickAssembler/Main.html#frontpage
[multipaint]: http://multipaint.kameli.net/
[petscii]: http://www.kameli.net/marq/?page_id=2717
