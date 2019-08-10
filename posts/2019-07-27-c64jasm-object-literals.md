---
title: C64jasm object literals
author: Janne Hellsten
public: true
syntax-css: syntax2.css  # use a newer syntax CSS for this file
#thumb: /images/c64/c64jasm/c64jasm-starter-example.png
---

The latest [c64jasm] v0.6.0 added support for JavaScript style object literals.  This turned out to be surprisingly useful!  Here we'll go over this feature and some tricks that it enables.

This post assumes you're already familiar with the c64jasm assembler (see: [c64jasm manual](https://nurpax.github.io/c64jasm/)).

## Object literals

An object in JavaScript and c64jasm is just a dict that maps keys to values.  For example, in JavaScript:

```
const options = {
    filename: "main.asm",
    indent: 4
};
```

The equivalent in c64jasm syntax would be:

```{.asm}
!let options = {
    filename: "main.asm",
    indent: 4
}
```

## How is this useful in assembler?

Turns out object literals can be quite useful for a few things:

- Named parameters for macro calls
- Keeping track of zeropage addresses
- Implicitly parametrize macro expansion

**Named parameters**: C64jasm supports only positional arguments in macro calls.  However, just like in JavaScript, objects are a great substitute for named parameters:

```{.asm}
!macro clear_screen_named(args) {
    lda #args.clearByte
    ldx #0
loop:
    !let screen = args.screen
    sta screen, x
    sta screen + $100, x
    sta screen + $200, x
    sta screen + $300, x
    inx
    bne loop
}

+clear_screen_named({ screen: $0400, clearByte: $a0 })
```

**Zero-page addresses**:  If you've written any decent amounts of 6502 assembly, you may have ran into problems keeping track of what's in the zeropage.  Perhaps you first started out by just keeping all the addresses in your head or code comments:

```{.asm}
    lda #0
    sta $20    ; sprite index
    lda #2
    sta $21    ; num sprites
```

This is hard to read and easily breaks on modification.  Instead, I tend to declare my zeropage allocation in variables:

```{.asm}
!let zp_sprite_idx = $20
!let zp_num_sprites = $21
    lda #0
    sta zp_sprite_idx
    lda #2
    sta zp_num_sprites
```

You can express the above equivalently using an object literal:

```{.asm}
!let zp = {
    sprite_idx: $20,
    num_sprites: $21
}
    lda #0
    sta zp.sprite_idx
    lda #2
    sta zp.num_sprites
```

Turns out, the latter form combines well with macro expansion.

## Macros and zeropage temporaries

Consider the below `mul_imm` macro that multiplies a 16-bit value by 3.  It needs 2 bytes of zeropage memory to hold a temporary value (`zp_tmp0`).  It's hardcoded to store the tempory in zeropage addresses `$20-$21`.

```{.asm}
!macro add16(res, n1, n2) {
    clc
    lda n1
    adc n2
    sta res+0
    lda n1+1
    adc n2+1
    sta res+1
}

!macro mul_imm(m, imm) {
    !let zp_tmp0 = $20
    !if (imm == 3) {
        +add16(zp_tmp0, m, m)
        +add16(m, zp_tmp0, m)
    } else {
        !error "only imm=3 is supported"
    }
}

func: {
    +mul_imm(num1, 3)
}
irq_func: {
    +mul_imm(num1, 3)  ; Ouch! Clobbers $20-21!
}

num1: !word 27
```

What if the code calling this macro is also using zeropage `$20-$21`?  The values in `$20-$21` will get clobbered by the macro.  You also probably cannot use this macro in an IRQ as the IRQ might then clobber `$20-21` while your main code is running and using the same memory.

We could of course pass in a 3rd parameter `zp_tmp0` that'd specify the temp zeropage location.  But this is ugly even with one 16-bit zeropage temp and only gets worse in macros needing many temporaries.

Let's parametrize zeropage temp locations in `mul_imm` by adding a `zp` macro argument that holds the zeropage allocation:

```{.asm}
!macro mul_imm(zp, m, imm) {
    !if (imm == 3) {
        +add16(zp.tmp0, m, m)
        +add16(m, zp.tmp0, m)
    }
}

func: {
    !let temps = { tmp0: $20 }
    +mul_imm(temps, num1, 3)
}
irq_func: {
    !let temps = { tmp0: $40 }
    +mul_imm(temps, num1, 3)  ; OK, no clobber
}
```

Better but still noisy.

~~Now here's the kicker: we can drop the `zp` argument from `mul_imm` declaration and rely on a convention that `zp` is passed implicitly in the enclosing scope at the macro call site.~~

~~In code:~~

```{.asm}
; no 'zp' arg here, rely on it being in scope
!macro mul_imm(m, imm) {
    !if (imm == 3) {
        +add16(zp.tmp0, m, m)
        +add16(m, zp.tmp0, m)
    }
}

; default zeropage temps
!let zp = { tmp0: $20 }

func: {
    ; use default zp tmp0=$20
    +mul_imm(num1, 3)
}
irq_func: {
    ; override zp with tmp0=$40 within irq_func
    !let zp = { tmp0: $40 }
    +mul_imm(num1, 3)
}
```

**Update 2019-08-09**: Implicit parameters by the sort of "dynamic scoping" shown here does not work as of c64jasm 0.7.0.  When a macro is expanded, any symbols in the macro will use bindings from where the macro was declared, not where it's expanded.  Use global variables instead.  See [this gist](https://gist.github.com/nurpax/5aeaba58e359c6d040a9e0f87fa68ab9) for an example on how to do this cleanly.


## Wrap-up

This post walked through a couple of tricks that became possible in the latest c64jasm v0.6.0 release.  If you're feeling adventurous, read more [here](https://nurpax.github.io/c64jasm/).  Or [try it in your browser](https://nurpax.github.io/c64jasm-browser/)!

[c64jasm]: https://nurpax.github.io/c64jasm/
