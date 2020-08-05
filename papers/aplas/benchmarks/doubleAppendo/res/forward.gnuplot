set datafile separator ','

set terminal pdf
set output 'forward.pdf'

set style data histogram
set style fill solid

set size ratio 1.1


set ylabel 'Time (ms)' 
set xlabel 'List length * 100' 

set key left top

set style line 100 lt 1 lc rgb "grey" lw 0.5 # linestyle for the grid
set grid ls 100 # enable grid with specific linestyle

plot 'forward.csv' u 5:xtic(1) title "Original", 'forward.csv' u 4:xtic(1) title "Ideal", 'forward.csv' u 3:xtic(1) title "ECCE", 'forward.csv' u 2:xtic(1) title "ConsPD", 'forward.csv' u 6:xtic(1) title "Geoff"

