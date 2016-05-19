#!/bin/sh

echo "Building purescript-conecanvas..."
cd ../purescript-OntoPanel
psc-bundle -m Main output/**/*.js -o ../cc-reddit/html/js/Main.js
cd ../cc-reddit

cabal run -- "$@"
