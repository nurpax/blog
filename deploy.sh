#!/bin/bash

cabal run blog rebuild
npx gh-pages --dist _site -b master --repo git@github.com:nurpax/nurpax.github.com
