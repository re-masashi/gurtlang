#include <stdio.h>
#include <time.h>
#include <stdlib.h>

long array_sum_helper(int* arr, int index, int size, long acc) {
    if (index >= size) {
        return acc;
    } else {
        return array_sum_helper(arr, index + 1, size, acc + arr[index]);
    }
}

long array_sum(int* arr, int size) {
    return array_sum_helper(arr, 0, size, 0);
}

int main(int argc, char* argv[]) {
    int iterations = 1;
    if (argc > 1) {
        iterations = atoi(argv[1]);
    }
    
    int arr[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    int size = 10;
    
    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start);
    
    long result = 0;
    for (int i = 0; i < iterations; i++) {
        result = array_sum(arr, size);
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
