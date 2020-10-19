# plot.plt from https://cyber.dabamos.de/programming/modernfortran/gnuplot.html
set terminal windows 0
set title "Derivative plot"
set grid
set xlabel "x"
set ylabel "y"
points="infoDerivatives.txt"
f(x)=3*x**2 + 6*x + 3
plot points using 1:2 title "2PF" with points, points using 1:3 title "2PB" with points, points using 1:4 title "3PE" with points, points using 1:5 title "3PM" with points, points using 1:6 title "5PE" with points, points using 1:7 title "5PM" with points, f(x) title "Derivative function"
