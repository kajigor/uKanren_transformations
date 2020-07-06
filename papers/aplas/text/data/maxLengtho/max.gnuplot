set datafile separator ','

set terminal pdf size 3.5, 3.5
set output 'max.pdf'

set style data histogram  
set style fill solid

set size ratio 1.1

set xrange [-0.5:1.75]
set yrange [0:55]

set ylabel 'Time (ms)' # label for the Y axis

set key left top

set style line 100 lt 1 lc rgb "grey" lw 0.5 # linestyle for the grid
set grid ls 100 # enable grid with specific linestyle

plot 'max.csv' u 2:xtic(1) title "Original", 'max.csv' u 3:xtic(1) title "Ideal removed",'max.csv' u 4:xtic(1) title "Ideal", 'max.csv' u 5:xtic(1) title "ECCE", 'max.csv' u 6:xtic(1) title "ConsPD"
