set term x11 font "-*-helvetica-medium-r-*-*-14-*-*-*-*-*-*-*"
set title "DE sol"
set nokey
set grid
set xlabel "x"
set ylabel "y"
set zlabel "z"
m="solutionValues.txt"
splot m using 1:2:3 with lines palette
