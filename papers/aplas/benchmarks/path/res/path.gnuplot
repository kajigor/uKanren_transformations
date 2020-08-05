set datafile separator ','

set terminal pdf
set output 'path.pdf'

set style data histogram
set style fill solid

set logscale y 10

set format y "10^%L"

set size ratio 1.1

set ylabel 'Time (ms)' # label for the Y axis
set xlabel 'isPath' # label for the X axis

set key left top

set style line 100 lt 1 lc rgb "grey" lw 0.5 # linestyle for the grid
set grid ls 100 # enable grid with specific linestyle

plot 'results.csv' u 2:xtic(1) title "Original", 'results.csv' u 3:xtic(1) title "ECCE", 'results.csv' u 4:xtic(1) title "ConsPD", 'results.csv' u 5:xtic(1) title "Geoff"

