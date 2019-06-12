---
title: C64 tool tricks (VICE and others)
author: Janne Hellsten
public: true
syntax-css: syntax2.css
---


A collection of random C64 tool tricks.

# VICE

## Capturing the ROM charset

Make ROM visible to the monitor and save data from `$d000` onwards:

```
bank rom
bs "data.bin" 0 $d000 $d7fff
```

# Using exomizer to crunch executables

## Installation

1. Download and extract [exomizer](https://bitbucket.org/magli143/exomizer/wiki/downloads/exomizer-3.0.2.zip)
2. Build it by

```
mkdir exomizer
cd exomizer
make -C src
```

The command line tools will reside under the `src` directory.

## Packing a .prg file

To pack a .prg that uses the BASIC upstart sequence (SYS something), use:

```
exomizer sfx sys input.prg -o output.prg -x3
```

The `-x3` adds a border flashing decrunching effect.  Drop that switch if you don't want it.
