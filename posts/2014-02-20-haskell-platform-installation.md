---
title: Installing Haskell Platform on Debian testing
author: Janne Hellsten
date: February 20, 2014
public: true
---

I usually hit some snags when installing Haskell Platform (HP) on
either Ubuntu or Debian.  I thought this time I would get lucky, and
get it installed without trouble.

## First Attempt

In anticipation of a great success, I first blew away all my local
installs of HP and GHC, followed by direct `haskell-platform` package
installation:

    apt-get install haskell-platform

D'oh, Debian testing's HP is at 2012.2.0.0.  Too old, I want a later
version.

HP 2013.2.0.0 requires GHC 7.6.3, so let's install that.  I follow the
links on [Haskell Platform for
Linux](http://www.haskell.org/platform/linux.html) and find the GHC
7.6.3 binary release.  Trying to install it according to its
installation steps..

Hmm, a problem with `libgmp.so.3`:

    ~/t/ghc-7.6.3$ ./configure 
    checking for path to top of build tree... utils/ghc-pwd/dist-install/build/tmp/ghc-pwd: 
    error while loading shared libraries: libgmp.so.3: cannot open shared object file: 
    No such file or directory

I try installing `libgmp.so.3`:

    apt-get install libgmp-dev

Oh, it's already at a newer version, i.e., at `libgmp.so.10`.

Googling..  Oh, symlink to fake that I have libgmp.so.3.  Sounds
dodgy.  Well, I'll install GHC from source instead.

So I download the GHC 7.6.3 source package, build it and install it.
At this point you need to have _some_ older version of GHC still
installed.  So I use GHC 7.4.1 from the Debian testing package.

With all this done, installing Haskell Platform from source is easy if
you managed to correctly install all the prerequisites.

Then I try to update my `cabal-install`, so I do:

    cabal update
    cabal install cabal-install

Whoops:


```
Building cabal-install-1.18.0.2...
Preprocessing executable 'cabal' for cabal-install-1.18.0.2...

Main.hs:118:8:
    Could not find module `Distribution.Version'
    There are files missing in the `Cabal-1.18.1.1' package,
    try running 'ghc-pkg check'.
    Use -v to see a list of the files searched for.
```

Even though I had blown away my `.cabal` and previous HP installation
directories from under my `$HOME`, I still had managed to leave in a
`.ghc` directory.  Nuking that fixed the last problem.

## Final Installation Steps

For my future reference, here are the high-level steps I took to
install the latest released Haskell Platform:

1. Prerequisites: some version of GHC installed.  In my case GHC 7.4.1
   from Debian.  Various other packages are needed for HP, these are
   easy to google so not listing them here.
2. Download GHC 7.6.3 source release, standard `./configure && make &&
   make install` installation
3. Check that `ghc --version` is 7.6.3.  If it's not, your PATH points
   to the previous GHC version.
    * Not sure if it's recommended to remove the previous GHC
      installation at this point.  Will I get into trouble with
      conflicting GHC versions if I don't remove it here?
    * Removed GHC 7.4.1 with `apt-get remove ghc` and `hash -r` to let
      bash know `ghc` is at a new location now.
4. Download the Haskell Platform 2013.2.0.0 source tarball from
   [haskell.org/platform/linux.html](http://www.haskell.org/platform/linux.html)
    * Install as per instructions while being careful to choose a
      global package database installation as opposed to a user local
      package db.  Otherwise you may face issues like [cabal-install
      #1695](https://github.com/haskell/cabal/issues/1695).

Phew, now it's all done.
