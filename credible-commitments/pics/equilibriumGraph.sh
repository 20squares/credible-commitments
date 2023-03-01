#!/bin/gnuplot -persist

set output 'EquilibriumGraph.gif'

set terminal gif transparent animate nooptimize delay 25 size 1500,1200

set key font "Monospace Regular, 18"

set grid xtics mxtics ytics mytics ztics mztics
set hidden3d back offset 1 trianglepattern 3 undefined 1 altdiagonal bentover
set title "Equilibria for players' swap sizes. Negative values denote swaps in the opposite direction." font "Monospace Regular, 22"

set xlabel "Player 1" offset 0,-1,0 rotate parallel
set xrange [ -50.0000 : 50.0000 ] noreverse nowriteback
set xtics scale 1.0 nomirror offset 0,-0.5,0

set ylabel "Player 2" offset 0,-1,0 rotate parallel
set yrange [ -50.0000 : 50.0000 ] noreverse nowriteback

set zlabel "Equilibrium Fee size" offset 0,-1,0 rotate parallel

set colorbox vertical origin screen 0.9, 0.2 size screen 0.05, 0.6 front  noinvert bdefault


$grid1 << EOD
11	-50	-40	-30	-20	-10	0	10	20	30	40	50
-50	16	14	11	8	4	0	0	0	0	0	0
-40	14	12	10	7	4	0	0	0	0	0	0
-30	11	10	8	6	3	0	0	0	0	0	0
-20	8	7	6	4	2	0	0	0	0	0	0
-10	4	4	3	2	1	0	0	0	0	0	0
0	0	0	0	0	0	0	0	0	0	0	0
10	0	0	0	0	0	0	1	2	3	4	4
20	0	0	0	0	0	0	2	4	6	7	8
30	0	0	0	0	0	0	3	6	8	10	11
40	0	0	0	0	0	0	4	7	10	12	14
50	0	0	0	0	0	0	4	8	11	14	16
EOD

$grid2 << EOD
11	-50	-40	-30	-20	-10	0	10	20	30	40	50
-50	15	13	10	7	3	0	0	0	0	0	0
-40	13	11	9	6	3	0	0	0	0	0	0
-30	10	9	7	5	2	0	0	0	0	0	0
-20	7	6	5	3	1	0	0	0	0	0	0
-10	3	3	2	1	0	0	0	0	0	0	0
0	0	0	0	0	0	0	0	0	0	0	0
10	0	0	0	0	0	0	0	1	2	3	3
20	0	0	0	0	0	0	1	3	5	6	7
30	0	0	0	0	0	0	2	5	7	9	10
40	0	0	0	0	0	0	3	6	9	11	13
50	0	0	0	0	0	0	3	7	10	13	15
EOD

n=60
do for [i=1:n]{

    set view 75, i*360/n
    splot '$grid1' matrix nonuniform with lines notitle lt rgb "#FF00FF", '$grid2' matrix nonuniform with lines notitle lt rgb "#AABBFF", '$grid1' matrix nonuniform with points lt rgb "#FF00AA" title "Minimum equilibrium", '$grid2' matrix nonuniform with points lt rgb "#AABBAA" title "Maximum equilibrium"
#    splot '$grid2' matrix nonuniform with points notitle
}
set output
