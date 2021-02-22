---
title: Debugging C64jasm projects with VICE and C64Debugger
author: Janne Hellsten
public: true
syntax-css: syntax2.css
---

This is a tutorial on how to export debug info from projects compiled with [c64jasm](https://nurpax.github.io/c64jasm/) for use with [VICE](https://vice-emu.sourceforge.io/) and [C64Debugger](https://sourceforge.net/projects/c64-debugger/).

C64jasm 0.9.1 added debug symbol output support for VICE and C64Debugger.  The debug info files come in two flavors:

- `--vice-moncommands-file`: VICE monitor commands file to add label names in VICE monitor.
- `--c64debugger-symbols-file`: KickAss debug symbols.  Maps PRG addresses to source locations enabling source level debugging.

Both flavors support adding breakpoints into assembly source with the `!break` directive.
