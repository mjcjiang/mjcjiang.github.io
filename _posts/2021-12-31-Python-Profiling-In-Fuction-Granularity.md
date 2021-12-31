Profiling is a technology to use some tools to check out some characteristics of a system. Use this tech
we can find the *hot spot* of the system, and do some refactor to remove the bottleneck of the system.Now
we check how to profile python code in function granularity.

# 1. What is cProfile:
cProfile and profile provide deterministic profiling of Python programs. A profile is a set of statistics that describes how often and for how long various parts of the program executed. These statistics can be formatted into reports via the *pstats* module.
# 2. Profile a function in a script:
Run the following code snippet:
``` python
import cProfile

def accsum(n):
    """acc sum"""
    res = 0
    for i in range(n):
        res += (i+1)
    return res

if __name__ == "__main__":
    cProfile.run('accsum(10000000)')
```

*accsum* is a function to calculate the cumulative sum of the number from 1 to n(n > 1), we use cProfile to profile this function, 
following is the running result:

``` bash
  4 function calls in 0.531 seconds

   Ordered by: standard name

   ncalls  tottime  percall  cumtime  percall filename:lineno(function)
        1    0.000    0.000    0.531    0.531 <string>:1(<module>)
        1    0.531    0.531    0.531    0.531 cpython_test.py:3(accsum)
        1    0.000    0.000    0.531    0.531 {built-in method builtins.exec}
        1    0.000    0.000    0.000    0.000 {method 'disable' of '_lsprof.Profiler' objects}
```

* ncalls: number of calls of the function[3/1 stand 1 call but 3 recur call]
* tottime: total time spend in the function, not include time spend in sub functions
* percall: tottime / calls
* cumtime: cumulative time spend in this function and subfunctions(even for recur functions)
* percall: cumtime / calls
* filename:lineno(function): which function in which file's which line

# 3. Use *pstat* to manipulate profile result:
Change privious code, this will generate a cumsum.stat file in current work directory:
``` python
if __name__ == "__main__":
    cProfile.run('accsum(10000000)', 'cumsum.stat')
```

Write a new script to analysize profile result.
``` python
import pstats
from pstats import SortKey

if __name__ == "__main__":
    p = pstats.Stats('cumsum.stat')
    p.strip_dirs().sort_stats(-1).print_stats()
    p.sort_stats(SortKey.NAME).print_stats()
    p.sort_stats(SortKey.CUMULATIVE).print_stats()
    p.sort_stats(SortKey.TIME).print_stats()
```

The running result is:
``` bash
Fri Dec 31 09:02:46 2021    cumsum.stat

         4 function calls in 0.531 seconds

   Ordered by: standard name

   ncalls  tottime  percall  cumtime  percall filename:lineno(function)
        1    0.000    0.000    0.531    0.531 <string>:1(<module>)
        1    0.531    0.531    0.531    0.531 cpython_test.py:3(accsum)
        1    0.000    0.000    0.531    0.531 {built-in method builtins.exec}
        1    0.000    0.000    0.000    0.000 {method 'disable' of '_lsprof.Profiler' objects}


Fri Dec 31 09:02:46 2021    cumsum.stat

         4 function calls in 0.531 seconds

   Ordered by: function name

   ncalls  tottime  percall  cumtime  percall filename:lineno(function)
        1    0.000    0.000    0.531    0.531 {built-in method builtins.exec}
        1    0.000    0.000    0.000    0.000 {method 'disable' of '_lsprof.Profiler' objects}
        1    0.000    0.000    0.531    0.531 <string>:1(<module>)
        1    0.531    0.531    0.531    0.531 cpython_test.py:3(accsum)


Fri Dec 31 09:02:46 2021    cumsum.stat

         4 function calls in 0.531 seconds

   Ordered by: cumulative time

   ncalls  tottime  percall  cumtime  percall filename:lineno(function)
        1    0.000    0.000    0.531    0.531 {built-in method builtins.exec}
        1    0.000    0.000    0.531    0.531 <string>:1(<module>)
        1    0.531    0.531    0.531    0.531 cpython_test.py:3(accsum)
        1    0.000    0.000    0.000    0.000 {method 'disable' of '_lsprof.Profiler' objects}


Fri Dec 31 09:02:46 2021    cumsum.stat

         4 function calls in 0.531 seconds

   Ordered by: internal time

   ncalls  tottime  percall  cumtime  percall filename:lineno(function)
        1    0.531    0.531    0.531    0.531 cpython_test.py:3(accsum)
        1    0.000    0.000    0.531    0.531 {built-in method builtins.exec}
        1    0.000    0.000    0.531    0.531 <string>:1(<module>)
        1    0.000    0.000    0.000    0.000 {method 'disable' of '_lsprof.Profiler' objects}
```

# 4. Profile a python script:
``` bash
python -m cProfile [-o output_file] [-s sort_order] (-m module | myscript.py)
```

* -o: wirte the profile result to a file
* -s: how to sort the profile result
* -m: module name

We add another function to the code snippet:
``` bash
import cProfile

def acc_sum(n):
    """acc sum"""
    res = 0
    for i in range(n):
        res += (i+1)
    return res

def acc_square_sum(n):
    """acc square sum"""
    res = 0
    for i in range(n):
        res += (i+1) * (i+1)
    return res

if __name__ == "__main__":
    acc_sum(1000000)
    acc_square_sum(1000000)
```

run follwing command:
``` bash
python -m cProfile -s tottime cpython_test.py
```
running result:
``` bash
         279 function calls (278 primitive calls) in 0.148 seconds

   Ordered by: internal time

   ncalls  tottime  percall  cumtime  percall filename:lineno(function)
        1    0.094    0.094    0.094    0.094 cpython_test.py:10(acc_square_sum)
        1    0.053    0.053    0.053    0.053 cpython_test.py:3(acc_sum)
        6    0.000    0.000    0.000    0.000 {built-in method nt.stat}
        1    0.000    0.000    0.000    0.000 {built-in method io.open_code}
        4    0.000    0.000    0.000    0.000 <frozen importlib._bootstrap_external>:1431(find_spec)
        1    0.000    0.000    0.000    0.000 {built-in method marshal.loads}
        1    0.000    0.000    0.000    0.000 {method 'read' of '_io.BufferedReader' objects}
        1    0.000    0.000    0.000    0.000 <frozen importlib._bootstrap_external>:969(get_data)
       20    0.000    0.000    0.000    0.000 <frozen importlib._bootstrap_external>:62(_path_join)
        1    0.000    0.000    0.000    0.000 {built-in method builtins.__build_class__}
       20    0.000    0.000    0.000    0.000 <frozen importlib._bootstrap_external>:64(<listcomp>)
        1    0.000    0.000    0.148    0.148 cpython_test.py:1(<module>)
```
OK! the result is sorted by *tottime*!


# Epilogue:
We can use *cProfile* to do a general profiling of a function or a program. But this is 
only in a function granularity, we need some more accurate profile tech. :), next is
line profiling.
