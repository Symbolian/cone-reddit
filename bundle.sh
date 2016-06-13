#!/bin/sh

echo "----- Building purescript-conecanvas"
cd ../purescript-OntoPanel
pulp build &&
psc-bundle -m Main output/**/*.js -o ../cone-reddit/html/js/Main.js &&
cd ../cone-reddit/html/js &&

echo "----- Minifying Javascript"
# Install uglify with `npm install uglify-js -g`
uglifyjs jQuery.js Main.js cone_reddit.js bootstrap.js -c -m -o cone_reddit.mini.js

cd ../..
echo "----- Done bundling"
