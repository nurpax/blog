---
title: Fixing Emacs GDB mode, part 1
author: Janne Hellsten
date: October 12, 2014
public: true
---

I like using use the Emacs `gud-mode` for my debugging sessions.
Unfortunately, it's pretty quirky in how it assigns source files to
Emacs windows.  This totally breaks my debugging flow. Today I decided
to document this misbehavior and in a later post, explain how to fix
it.

The problem occurs when stepping through a program in `gud-mode` with
`gdb-many-windows` enabled.  I'd expect Emacs to assign new source
buffers into the dedicated middle-left source window.  Instead, source
buffers are (randomly?) assigned on top of other GDB buffers like the
GDB comint window.  The layout problem gets even worse with multiple
Emacs frames.  Here are my repro steps with a diagram of what happens
and how I'd like Emacs to behave instead.

Here are the steps to reproduce the problem on Emacs 24.3.1.

Bind `gud-next` and `gud-step` to `[f7]` and `[f8]`:

```{.lisp}
;; This goes into your .emacs
(add-hook 'gud-mode-hook
          '(lambda ()
             (global-set-key (kbd "<f7>") 'gud-next)
             (global-set-key (kbd "<f8>") 'gud-step)))
```


Build my example program:

```{.bash}
git clone https://gist.github.com/d94e2666ae05b58906c6.git emacs-gdb-proj
cd emacs-gdb-proj
make
```

Start Emacs and enter GDB:

```
emacs &
M-x gdb
M-x gdb-many-windows
```


Use `*gud-test*` comint buffer to set a break point in `main()` and run the program:

```
(gdb) b main
Breakpoint 1 at 0x4004b1: file main.c, line 6.
(gdb) r
```

The window layout should now look something like this (the source window with `main.c` highlighted):

<svg width="600" height="400">
<!-- gud-test, locals of test -->
  <g transform="translate(0 0)">
    <rect width="50%" height="33%" style="fill:rgb(0,43,53); stroke-width:2; stroke:rgb(0,0,0)" />
    <text x="5%" y="18%" font-size="20" font-family="Helvetica" fill="rgb(131,148,150)">&#42;gud-test&#42;</text>
  </g>
  <g transform="translate(300 0)">
    <rect width="50%" height="33%" style="fill:rgb(0,43,53); stroke-width:2; stroke:rgb(0,0,0)" />
    <text x="5%" y="18%" font-family="Helvetica" font-size="20" fill="rgb(131,148,150)">&#42;locals of test&#42;</text>
  </g>
<!-- source window, output window -->
  <g transform="translate(0 133)">
    <rect width="50%" height="33%" style="fill:rgb(101,123,131); stroke-width:2; stroke:rgb(0,0,0)" />
    <text x="5%" y="18%" font-family="Helvetica" font-size="20" fill="rgb(0,43,53)">./main.c</text>
  </g>
  <g transform="translate(300 133)">
    <rect width="50%" height="33%" style="fill:rgb(0,43,53); stroke-width:2; stroke:rgb(0,0,0)" />
    <text x="5%" y="18%" font-family="Helvetica" font-size="20" fill="rgb(131,148,150)">&#42;input/output of test&#42;</text>
  </g>
<!-- stack frame, breakpoints -->
  <g transform="translate(0 266)">
    <rect width="50%" height="33%" style="fill:rgb(0,43,53); stroke-width:2; stroke:rgb(0,0,0)" />
    <text x="5%" y="18%" font-family="Helvetica" font-size="20" fill="rgb(131,148,150)">&#42;stack frames of test&#42;</text>
  </g>
  <g transform="translate(300 266)">
    <rect width="50%" height="33%" style="fill:rgb(0,43,53); stroke-width:2; stroke:rgb(0,0,0)" />
    <text x="5%" y="18%" font-family="Helvetica" font-size="20" fill="rgb(131,148,150)">&#42;breakpoints of test&#42;</text>
  </g>
</svg>

The contents of `main.c` look like this:

```{.c}
#include <stdio.h>

int main() 
{
    foo();
    foo();
    foo();
}
```

If you step into the `foo()` function with `[f8]`, you're taken into
the `b.c` source file, which opens as expected into left-middle source
window.  

However, if instead of directly stepping into `foo()` you open another
source file in the source window, say, `C-x C-f`'ing the `Makefile`
from the test project and hit `[f8]` again, BOOM, the `b.c` buffer
opens in the comint `*gud-test*` window and not in the source window!
The window layout now looks like this:

<svg width="600" height="400">
<!-- b.c, locals of test -->
  <g transform="translate(0 0)">
    <rect width="50%" height="33%" style="fill:rgb(101,123,131); stroke-width:2; stroke:rgb(0,0,0)" />
    <text x="5%" y="18%" font-family="Helvetica" font-size="20" fill="rgb(0,43,53)">./b.c</text>
  </g>
  <g transform="translate(300 0)">
    <rect width="50%" height="33%" style="fill:rgb(0,43,53); stroke-width:2; stroke:rgb(0,0,0)" />
    <text x="5%" y="18%" font-family="Helvetica" font-size="20" fill="rgb(131,148,150)">&#42;locals of test&#42;</text>
  </g>
<!-- source window, output window -->
  <g transform="translate(0 133)">
    <rect width="50%" height="33%" style="fill:rgb(101,123,131); stroke-width:2; stroke:rgb(0,0,0)" />
    <text x="5%" y="18%" font-family="Helvetica" font-size="20" fill="rgb(0,43,53)">./Makefile</text>
  </g>
  <g transform="translate(300 133)">
    <rect width="50%" height="33%" style="fill:rgb(0,43,53); stroke-width:2; stroke:rgb(0,0,0)" />
    <text x="5%" y="18%" font-family="Helvetica" font-size="20" fill="rgb(131,148,150)">&#42;input/output of test&#42;</text>
  </g>
<!-- stack frame, breakpoints -->
  <g transform="translate(0 266)">
    <rect width="50%" height="33%" style="fill:rgb(0,43,53); stroke-width:2; stroke:rgb(0,0,0)" />
    <text x="5%" y="18%" font-family="Helvetica" font-size="20" fill="rgb(131,148,150)">&#42;stack frames of test&#42;</text>
  </g>
  <g transform="translate(300 266)">
    <rect width="50%" height="33%" style="fill:rgb(0,43,53); stroke-width:2; stroke:rgb(0,0,0)" />
    <text x="5%" y="18%" font-family="Helvetica" font-size="20" fill="rgb(131,148,150)">&#42;breakpoints of test&#42;</text>
  </g>
</svg>

Another way to trigger the same:

1. Start `gud-mode` + `gdb-many-windows` as above.
2. Step into `foo()`.
3. Open the `Makefile` into the source window.
4. Click on any of the functions/source files into bottom-left stack frame window.

Again, the source file opens on top of the upper-left comint window.

I did find some related Stackoverflow posts.  [This post][1] contains
a `defadvice` trick that seems to work for the latter reproduction
steps, but it didn't make a difference in the former case.  [Another
post][2] discusses a related problem but concludes that there's no
problem if you just use `gdb-many-windows`.

I hope to resolve this soon and update this post with a solution.

 [1]: http://stackoverflow.com/questions/3473134/emacs-23-1-1-with-gdb-forcing-source-windows
 [2]: http://stackoverflow.com/questions/24386672/use-gdb-within-emacs-always-show-the-source-code.
