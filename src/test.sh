cp appendo2.ml test
cp reverso.ml test
cp revacco.ml test 

cd test 
make clean
make TOPFILE=test
./test.opt
