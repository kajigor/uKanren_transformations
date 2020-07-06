set datafile separator ','

set terminal pdf size 3.5, 2.8
set output 'da700.pdf'

set style data histogram  
set style fill solid

set size ratio 0.8

set xrange [0.5:2.75]

set ylabel 'Time (ms)' # label for the Y axis
set xlabel 'Direction' # label for the X axis

set key left top

set style line 100 lt 1 lc rgb "grey" lw 0.5 # linestyle for the grid
set grid ls 100 # enable grid with specific linestyle

plot 'da700.csv' u 2:xtic(1) title "Original", 'da700.csv' u 3:xtic(1) title "Ideal",'da700.csv' u 4:xtic(1) title "ECCE", 'da700.csv' u 5:xtic(1) title "ConsPD"
