#include <stdio.h>
#include <time.h>
#include <stdlib.h>

long fib(long n) {
    if (n <= 1) {
        return n;
    } else {
        return fib(n - 1) + fib(n - 2);
    }
}

int main(int argc, char* argv[]) {
    int iterations = 1;
    if (argc > 1) {
        iterations = atoi(argv[1]);
    }
    
    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start);
    
    long result = 0;
    for (int i = 0; i < iterations; i++) {
        result = fib(35);
    }
    
    clock_gettime(CLOCK_MONOTONIC, &end);
    
    printf("Result: %ld\n", result);
    printf("Iterations: %d\n", iterations);
    printf("Time: %.6f seconds\n", 
           (end.tv_sec - start.tv_sec) + (end.tv_nsec - start.tv_nsec) / 1e9);
    printf("Avg per iteration: %.6f seconds\n",
           ((end.tv_sec - start.tv_sec) + (end.tv_nsec - start.tv_nsec) / 1e9) / iterations);
    
    return 0;
}
