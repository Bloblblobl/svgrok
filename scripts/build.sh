rm -rf ./build
elm make src/Main.elm --output=./build/index.js --optimize
cp ./public/index.html ./build/index.html
cp ./public/style.css ./build/style.css