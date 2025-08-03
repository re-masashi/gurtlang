import time
import sys

def array_sum_helper(arr, index, size, acc):
    if index >= size:
        return acc
    else:
        return array_sum_helper(arr, index + 1, size, acc + arr[index])

def array_sum(arr, size):
    return array_sum_helper(arr, 0, size, 0)

if __name__ == "__main__":
    iterations = 1
    if len(sys.argv) > 1:
        iterations = int(sys.argv[1])
    
    arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    size = 10
    
    start = time.time()
    
    result = 0
    for i in range(iterations):
        result = array_sum(arr, size)
    
    end = time.time()
    
    print(f"Result: {result}")
    print(f"Iterations: {iterations}")
    print(f"Time: {end - start:.6f} seconds")
    print(f"Avg per iteration: {(end - start) / iterations:.6f} seconds")
