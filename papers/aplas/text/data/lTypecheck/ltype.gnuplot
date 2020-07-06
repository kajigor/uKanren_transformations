set datafile separator ','

set terminal pdf 
set output 'ltype.pdf'

set size ratio 0.8

set terminal pdf size 3, 2.4 

set style data histogram  
set style fill solid

set ylabel 'Time (ms)' # label for the Y axis
set xlabel 'Direction' # label for the X axis

set xrange [0.5:2.5]
set yrange [0.1:]

set key left top

set style line 100 lt 1 lc rgb "grey" lw 0.5 # linestyle for the grid
set grid ls 100 # enable grid with specific linestyle

plot 'ltype.csv' u 2:xtic(1) title "Original", 'ltype.csv' u 3:xtic(1) title "ECCE", 'ltype.csv' u 4:xtic(1) title "ConsPD"
