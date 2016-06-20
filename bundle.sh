#!/bin/sh

echo "----- Building purescript-conecanvas"
cd ../purescript-OntoPanel
pulp build &&
psc-bundle -m Main output/**/*.js -o ../cone-reddit/assets/Main.js &&
cd -

echo "----- Minifying Javascript"
# Install uglify with `npm install uglify-js -g`
cd assets
uglifyjs jQuery.js Main.js cone_reddit.js bootstrap.js offline.js hammer.js -c -m -o cone_reddit.mini.js &&
mv ./cone_reddit.mini.js ../html/js/
cd -

echo "----- Done bundling"
