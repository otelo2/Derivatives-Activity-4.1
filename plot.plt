# plot.plt from https://cyber.dabamos.de/programming/modernfortran/gnuplot.html
set terminal windows 0
set title "Derivative plot"
set nokey
set grid
set xlabel "x"
set ylabel "y"
m="infoDerivatives3PE.txt"
plot m using 1:2 with linespoints