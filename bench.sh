#!/bin/bash

ITERATIONS=1

echo "=== Fibonacci Benchmark (n=35, $ITERATIONS iterations) ==="

echo "Compiling C version..."
gcc -O3 -o examples/fib_c examples/fib.c -lrt

echo "Running C version:"
./examples/fib_c $ITERATIONS

echo ""
echo "Running Python version:"
python3 examples/fib.py $ITERATIONS

echo ""
echo "Compiling and running Gurt version:"
cargo run -- examples/fib.gurt --bench --iterations $ITERATIONS

echo ""
echo "=== Array Sum Benchmark ($ITERATIONS iterations) ==="

echo "Compiling C version..."
gcc -O3 -o examples/array_c examples/array_sum.c -lrt

echo "Running C version:"
./examples/array_c $ITERATIONS

echo ""
echo "Running Python version:"
python3 examples/array_sum.py $ITERATIONS

echo ""
echo "Compiling and running Gurt version:"
cargo run -- examples/array_sum.gurt --bench --iterations $ITERATIONS
