 cd core
find ./ -type f -exec sed -i -e 's/MinEmacs/RivenEmacs/g' {} \;   

 cd ../elisp
find ./ -type f -exec sed -i -e 's/MinEmacs/RivenEmacs/g' {} \;  
 cd ../modules
find ./ -type f -exec sed -i -e 's/MinEmacs/RivenEmacs/g' {} \;  
 cd ../skel
find ./ -type f -exec sed -i -e 's/MinEmacs/RivenEmacs/g' {} \;  
 exit 0


cd core
find ./ -type f -exec sed -i -e 's/nare\-/name\-/g' {} \;   
find ./ -type f -exec sed -i -e 's/mire\-/mime\-/g' {} \;
find ./ -type f -exec sed -i -e 's/gare\-/game\-/g' {} \;   
find ./ -type f -exec sed -i -e 's/sore\-/some\-/g' {} \;
find ./ -type f -exec sed -i -e 's/frare\-/frame\-/g' {} \;   
find ./ -type f -exec sed -i -e 's/there\-/theme\-/g' {} \;

cd ../elisp
find ./ -type f -exec sed -i -e 's/nare\-/name\-/g' {} \;   
find ./ -type f -exec sed -i -e 's/mire\-/mime\-/g' {} \;
find ./ -type f -exec sed -i -e 's/gare\-/game\-/g' {} \;   
find ./ -type f -exec sed -i -e 's/sore\-/some\-/g' {} \;
find ./ -type f -exec sed -i -e 's/frare\-/frame\-/g' {} \;   
find ./ -type f -exec sed -i -e 's/there\-/theme\-/g' {} \;

cd ../modules
find ./ -type f -exec sed -i -e 's/nare\-/name\-/g' {} \;   
find ./ -type f -exec sed -i -e 's/mire\-/mime\-/g' {} \;
find ./ -type f -exec sed -i -e 's/gare\-/game\-/g' {} \;   
find ./ -type f -exec sed -i -e 's/sore\-/some\-/g' {} \;
find ./ -type f -exec sed -i -e 's/frare\-/frame\-/g' {} \;   
find ./ -type f -exec sed -i -e 's/there\-/theme\-/g' {} \;


cd ../skel
find ./ -type f -exec sed -i -e 's/nare\-/name\-/g' {} \;   
find ./ -type f -exec sed -i -e 's/mire\-/mime\-/g' {} \;
find ./ -type f -exec sed -i -e 's/gare\-/game\-/g' {} \;   
find ./ -type f -exec sed -i -e 's/sore\-/some\-/g' {} \;
find ./ -type f -exec sed -i -e 's/frare\-/frame\-/g' {} \;   
find ./ -type f -exec sed -i -e 's/there\-/theme\-/g' {} \;

cd ..
sed -i -e 's/nare\-/name\-/g' ./init.el   
sed -i -e 's/mire\-/mime\-/g' ./init.el
sed -i -e 's/gare\-/game\-/g' ./init.el
sed -i -e 's/sore\-/some\-/g' ./init.el
sed -i -e 's/frare\-/frame\-/g' ./init.el 
sed -i -e 's/there\-/theme\-/g' ./init.el

sed -i -e 's/nare\-/name\-/g' ./early-init.el   
sed -i -e 's/mire\-/mime\-/g' ./early-init.el
sed -i -e 's/gare\-/game\-/g' ./early-init.el
sed -i -e 's/sore\-/some\-/g' ./early-init.el
sed -i -e 's/frare\-/frame\-/g' ./early-init.el 
sed -i -e 's/there\-/theme\-/g' ./early-init.el
