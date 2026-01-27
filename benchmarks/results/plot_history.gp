# Gnuplot script for benchmark history
set terminal dumb 120 30
set title 'Benchmark History'
set xlabel 'Time'
set ylabel 'Time (us)'
set xdata time
set timefmt '%s'
set format x '%m/%d'
set key outside right
plot 'benchmarks/results/benchmark_history.dat' using 1:2 with linespoints title 'pattern_matching/nested_seq_02_children', \
     'benchmarks/results/benchmark_history.dat' using 1:3 with linespoints title 'pattern_matching/nested_seq_05_children', \
     'benchmarks/results/benchmark_history.dat' using 1:4 with linespoints title 'pattern_matching/nested_seq_10_children', \
     'benchmarks/results/benchmark_history.dat' using 1:5 with linespoints title 'pattern_matching/sequence_02_children', \
     'benchmarks/results/benchmark_history.dat' using 1:6 with linespoints title 'pattern_matching/sequence_05_children', \
     'benchmarks/results/benchmark_history.dat' using 1:7 with linespoints title 'pattern_matching/sequence_10_children', \
     'benchmarks/results/benchmark_history.dat' using 1:8 with linespoints title 'pattern_matching/simple_pattern'
