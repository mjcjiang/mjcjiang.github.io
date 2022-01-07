Sometimes you need to testify your program's memory usage for some specific reasons,
it's the *memory-profiler* come to rescure. Now that you can see the memory-usage change in a
*line-by-line* mode, so you can check which line cost the most memory-usage. Also
you can see the change in a time-line mode, to get a big picture of the memory-usage in 
the program's lifetime. Do memory profiling could be very slow compare to cpu profiling, be careful! 

# 1. Package install:
pip install -U memory_profiler

# 2. Profile line-by-line memory usage:
Example Code: mem_prifile.py
``` python
@profile
def my_func():
    a = [1] * (10 ** 6)
    b = [2] * (2 * 10 ** 7)
    del b
    return a

if __name__ == '__main__':
    my_func()
```

Run profile:
``` bash
python -m memory_profiler mem_profile.py
```

Result:
``` bash
Filename: mem_profile.py
Line #    Mem usage    Increment  Occurrences   Line Contents
=============================================================
     1   40.422 MiB   40.422 MiB           1   @profile
     2                                         def my_func():
     3   48.055 MiB    7.633 MiB           1       a = [1] * (10 ** 6)
     4  200.645 MiB  152.590 MiB           1       b = [2] * (2 * 10 ** 7)
     5   48.055 MiB -152.590 MiB           1       del b
     6   48.055 MiB    0.000 MiB           1       return a
```
* Mem usage: after this line of code, the memory usage of the program(process).
* Increment: how many memory difference caused by this line of code.
In the privious profile result, we can see *del b* cause a big memory-usage decline, because
we just free it out.

# 3. Profile with decorator:
Code:
``` python
from memory_profiler import profile

@profile
def my_func():
    a = [1] * (10 ** 6)
    b = [2] * (2 * 10 ** 7)
    del b
    return a
	
if __name__ == '__main__':
    my_func()
```

Run profile(without -m mempory_profiler):
``` bash
python mem_profile.py
```

Running result:
``` bash
Filename: mem_profile.py
Line #    Mem usage    Increment  Occurrences   Line Contents
=============================================================
     3     40.4 MiB     40.4 MiB           1   @profile
     4                                         def my_func():
     5     48.0 MiB      7.6 MiB           1       a = [1] * (10 ** 6)
     6    200.6 MiB    152.6 MiB           1       b = [2] * (2 * 10 ** 7)
     7     48.0 MiB   -152.6 MiB           1       del b
     8     48.0 MiB      0.0 MiB           1       return a
```
The result is just the same as the line-by-line mode.

# 4. Time-based memory usage:
Sometimes it is useful to have full memory usage reports as a function of time (not line-by-line) 
of external processes (be it Python scripts or not). In this case the executable mprof might be useful.
Code:
``` python
@profile
def my_func():
    a = [1] * (10 ** 6)
    b = [2] * (2 * 10 ** 7)
    del b
    return a

@profile
def your_func():
    c = [1] * (10 ** 6)
    d = [2] * (2 * 10 ** 7)
    del d
    return c

if __name__ == '__main__':
    my_func()
    your_func()
```

Run time-based profile:
``` bash
mprof run mem_profile.py
```

This is the command output:
```
mprof: Sampling memory every 0.1s
running new process
running as a Python program...
```

Now do memory usage plot:
```bash
mprof plot --flame
```

This is the memory profile graph:
![mem_profile_graph](./pics/memprofile/mem_profile.png)

# 5. Epilogue:
Memory profile can be very slow if your program's memory-usage is high. But
it's a very good tool to inspect your program's behavior, the more you know
your program's behavior, the more you can find its bugs or constraints. Enjoy!
