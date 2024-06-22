# set pm3d map
# set pm3d interpolate 2,2
# splot 'solutionPoints.txt' matrix


set term x11 font "-*-helvetica-medium-r-*-*-14-*-*-*-*-*-*-*"
set title "DE sol - RK4"
set nokey
set grid
set xlabel "x"
set ylabel "y"
# set pm3d
m="solutionValues.txt"
splot m matrix