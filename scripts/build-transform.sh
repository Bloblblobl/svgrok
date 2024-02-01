rm -rf ./build
elm make src/Transform.elm --output=./build/transform-svg_elm.js --optimize
cp ./public/transform-svg.js ./build/transform-svg.js
