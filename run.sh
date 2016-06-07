#!/bin/sh

echo "Building purescript-conecanvas..."
cd ../purescript-OntoPanel
pulp build &&
psc-bundle -m Main output/**/*.js -o ../cone-reddit/html/js/Main.js &&
cd ../cone-reddit &&

cabal run -- "$@"
