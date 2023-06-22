BuildDir=$(ls -td ./local/straight/build-29*/ | head -1)

echo $BuildDir

cd $BuildDir

cd lsp-bridge

find . -type f -name "*.elc" | xargs rm 

cd ..
cd blink-search
find . -type f -name "*.elc" | xargs rm


cd ..
cd color-rg
find . -type f -name "*.elc" | xargs rm
