rm ocanrenize *.hi *.o appendo2.ml reverso.ml revacco.ml minmaxo.ml gto.ml test/gto.ml leo.ml test/leo.ml smallesto.ml test/smallesto.ml test/appendo2.ml test/reverso.ml test/revacco.ml test/minmaxo.ml

ghc -o ocanrenize -main-is OCanrenize OCanrenize.hs
./ocanrenize

cp appendo2.ml test
cp reverso.ml test
cp revacco.ml test 
cp gto.ml test 
cp leo.ml test
cp smallesto.ml test
cp minmaxo.ml test

cd test 
make clean
make TOPFILE=test
./test.opt
