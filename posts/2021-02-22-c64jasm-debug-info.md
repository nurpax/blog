---
title: Debugging C64jasm projects with VICE and C64Debugger
author: Janne Hellsten
public: true
syntax-css: syntax2.css
---

This is a tutorial on how to export debug info from projects compiled with [c64jasm](https://nurpax.github.io/c64jasm/) for use with [VICE](https://vice-emu.sourceforge.io/) and [C64Debugger](https://sourceforge.net/projects/c64-debugger/).

C64jasm 0.9.1 added debug symbol output support for VICE and C64Debugger.  The debug info files come in two flavors:

- `--vice-moncommands`: VICE monitor commands file to show label names in the monitor.
- `--c64debugger-symbols`: KickAss debug symbols for C64Debugger.  Maps program addresses to source locations enabling source level debugging.

Both flavors support adding breakpoints into assembly source with the `!break` directive.

# VICE monitor commands

To enable easier debugging with VICE, you can use the `--vice-moncommands` option to generate a program address to label name mapping script ("moncommands").  This script also includes setting breakpoints if your source files had any `!break` lines.

Here's how to output and use a moncommands script.  You can try this with the c64jasm git repository or your own assembly project.

```
git clone https://github.com/nurpax/c64jasm.git
cd c64jasm/examples/content-pipe
```

Edit src/main.asm and add a `!break` line as below:

```{.asm}
+basic_start(entry)
;--------------------------------------------------------------
; Execution starts here
;--------------------------------------------------------------
entry: {
    !break          ; 👈 add breakpoint here!
    lda #0
    jsr music.init

    sei
    lda #$35        ; Bank out kernal and basic
    sta $01         ; $e000-$ffff
    +setup_irq(irq_top, irq_top_line)
    cli
```

Then compile the project with this command:

```
c64jasm src/main.asm --vice-moncommands=main.vs --out main.prg
```

This should produce two files: `main.prg` and `main.vs`.  The contents of `main.vs` should look something like this:

```
al C:080d .entry
al C:0854 .entry::frame_loop
al C:0857 .entry::vsync
al C:0868 .animate_sprites
al C:086d .animate_sprites::anim_sprites
al C:0891 .animate_sprites::anim_sprites_x
[bunch of lines omitted..]
al C:20e8 .pet_rle::decode::decode_line::dst
al C:20f2 .background_petscii_rle
```

You can use these files in VICE like so:

```
x64 -moncommands main.vs main.prg
```

If everything went fine, VICE should break into the monitor at the start of the example project.  Depending on your VICE build and configuration, you will see the monitor either in a separate window (preferred) or in the VICE C64 window like on my MBP:

<div class="center">
<img width="100%" class="img-pixelated" src="/images/c64/c64jasm/vice-monitor-disasm.png" />
</div>

You can step through instructions with `step`, disassemble from the current program counter with `disass` or continue execution with `g`.

TODO enter monitor with keyboard shortcut.
