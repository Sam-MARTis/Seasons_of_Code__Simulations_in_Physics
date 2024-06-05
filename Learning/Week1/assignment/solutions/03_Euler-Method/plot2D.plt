set term x11 font "-*-helvetica-medium-r-*-*-14-*-*-*-*-*-*-*"
set title "DE sol"
set nokey
set grid
set xlabel "x"
set ylabel "y"
m="solutionValues.txt"
plot m using 1:2 with linespoints
