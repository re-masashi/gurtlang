import time
import sys

def fib_helper(n: int, a: int, b: int):
    if n <= 1:
        return a
    else:
        return fib_helper(n - 1, b, a + b)

def fib(n: int):
    return fib_helper(n, 0, 1)

if __name__ == "__main__":
    iterations = 1
    if len(sys.argv) > 1:
        iterations = int(sys.argv[1])
    
    start = time.time()
    
    result = 0
    for i in range(iterations):
        result = fib(35)
    
    end = time.time()
    
    print(f"Result: {result}")
    print(f"Iterations: {iterations}")
    print(f"Time: {end - start:.6f} seconds")
    print(f"Avg per iteration: {(end - start) / iterations:.6f} seconds")
