Sometime when function level profiling is not enough to locate the bottleneck of a function(Some very long function), we
need to do line profiling. The *line-profiler* is comming to rescure.

# 1. Install:
``` bash
pip install line_profiler
```

# 2. Do line profile for a function:
We construct a script with a *slow_fuction* in it:
``` python
def acc_square_add(n):
    """acc square sum"""
    res = 0
    for i in range(n):
        res += (i+1) * (i+1)
    return res

@profile
def slow_function():
    print("slow function begin")
    acc_square_add(10000000)
    print("slow function end")

if __name__ == "__main__":
    slow_function()
```
We want to find the bottleneck of the *slow_function*.

Run the following command:
``` bash
kernprof -l -v line_profiler_test.py
```
"-v" will show the result in the stdout, this is the result:

``` bash
slow function begin
slow function end
Wrote profile results to line_profiler_test.py.lprof
Timer unit: 1e-06 s

Total time: 2.51533 s
File: line_profiler_test.py
Function: slow_function at line 8

Line #      Hits         Time  Per Hit   % Time  Line Contents
==============================================================
     8                                           @profile
     9                                           def slow_function():
    10         1         84.0     84.0      0.0      print("slow function begin")
    11         1    2515077.9 2515077.9    100.0      acc_square_add(10000000)
    12         1        169.8    169.8      0.0      print("slow function end")
```

You can easily find *acc_square_add* is the bottleneck of our *slow_function*.
Also kernprof will generate a *.lprof* in current working directory, we can 
check it use the following command:

``` bash
python -m line_profiler line_profiler_test.py.lprof
```

The running result:
``` bash
Timer unit: 1e-06 s

Total time: 2.51533 s
File: line_profiler_test.py
Function: slow_function at line 8

Line #      Hits         Time  Per Hit   % Time  Line Contents
==============================================================
     8                                           @profile
     9                                           def slow_function():
    10         1         84.0     84.0      0.0      print("slow function begin")
    11         1    2515077.9 2515077.9    100.0      acc_square_add(10000000)
    12         1        169.8    169.8      0.0      print("slow function end")
```
Same as the privious running result!

# 3. Epilogue:
line-profiler and kernprof may do you a lot of good, just try them! :） bye!
