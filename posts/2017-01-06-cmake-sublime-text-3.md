---
title: CMake builds from within Sublime Text 3
author: Janne Hellsten
date: Jan 6, 2017
public: true
---

How to setup a C++11 project with CMake and make it buildable from within a Sublime Text 3 project.

Example project can be found [here][gist].

## Step 1: Create C++ and CMake files

Create a C++ source file:

```{.c}
#include <stdio.h>

int main()
{
    printf("Hello, world!\n");
    return 0;
}
```

Create a CMakeLists.txt to build your C++ file:

```
cmake_minimum_required (VERSION 2.6)
project (example)
set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
add_executable(example main.cpp)
```

## Step 2:

Use CMake to generate your makefiles.  We put these temporaries under a `./build` directory.

```{.bash}
mkdir build
cd build
cmake ..
```

If you now run `make` in the `./build` directory, your project should build.

## Step 3: Create your ST3 project

Create a `example.sublime-project` file for your project.

Edit the project file and add a build system (see "build_systems"):

```{.json}
{
    "folders":
    [
        {
            "path": "."
        }
    ],
    "build_systems":
    [
        {
            "name": "cmake",
            "shell_cmd": "make -C build",
            "file_regex": "/([^/:]+):(\\d+):(\\d+): "
        }
    ]
}
```

This will add a "cmake" build system into ST3 when you open this project file.  The convention used is to put the CMake generated makefiles under ./build, and thus the build command uses `make -C build` instead of just `make`.

## Step 4: Build from within ST3

Load the example project into ST3.

You should be able to choose `cmake` as your current build system from ST3 Tools/Build System menu.

If you now hit Ctrl-Shift-B (or ⌘ + ⇧ + V), your project should build and you should see the build output in bottom console window.

If you encountered build errors, you can go to next/prev error with F4 and Shift-F4.

## Full source code

[Project gist][gist], embedded below.

<script src="https://gist.github.com/nurpax/1927b189bccdac1fbf6389a47c8a7fab.js"></script>

[gist]: https://gist.github.com/nurpax/1927b189bccdac1fbf6389a47c8a7fab
