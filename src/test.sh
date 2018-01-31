rm ocanrenize *.hi *.o appendo2.ml reverso.ml revacco.ml test/appendo2.ml test/reverso.ml test/revacco.ml

ghc -o ocanrenize -main-is OCanrenize OCanrenize.hs
./ocanrenize

cp appendo2.ml test
cp reverso.ml test
cp revacco.ml test 

cd test 
make clean
make TOPFILE=test
./test.opt
