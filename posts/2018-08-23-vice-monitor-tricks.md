---
title: C64 VICE monitor commands
author: Janne Hellsten
public: true
---


A collection of random VICE monitor commands.

Capturing the ROM charset
-------------------------

Make ROM visible to the monitor and save data from `$d000` onwards:

```
bank rom
bs "data.bin" 0 $d000 $d7fff
```
