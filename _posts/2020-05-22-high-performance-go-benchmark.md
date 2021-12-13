# Benchmark Your Code
when you want know the throughput of a database, you do benchmark;
when you want to know the delay time of net package, you do benchmark;
when you want to know the performance of a function write in some language, you do benchmark;
maybe when god was creating this world, he/she did so much benchmarking.:)
now let\'s see how to do benchmark in golang.

## 1. prepare benchmark environment
* the mechine must be idle;
* be careful with power saving and thermal scaling;
* do not use virtual mechine and shared cloud hosting;
	
## 2. benchmarking using golang *testing* package:
let\'s use a simple function for example, the following Fib function is raw and too badlly slow!
    
benching.go:
	
``` go
func Fib2(n int) int {
	switch n {
	case 0:
		return 0
	case 1:
		return 1
	default:
		return Fib2(n-1) + Fib2(n-2)
	}
}
``` 

benching_test.go:
	
``` go
func BenchmarkFib20(b *testing.B) {
	for n := 0; n < b.N; n++ {
		Fib2(20)
	}
}

func BenchmarkFib28(b *testing.B) {
	for n := 0; n < b.N; n++ {
		Fib2(28)
	}
}
``` 

run *go test* command to do the real benching work:
	
``` bash
    $go test -bench=. .
``` 
the privious command run all the benching functions under current directory. you can also run specific benching functions:
	
``` bash
    $go test -bench=Fib20 .
``` 
the privious command run specific benching function which name contain "Fib20";  
Tips: if you use emacs do go development, try add the following code snippet into your emacs config file:
``` lisp
(defun get-word-on-point()
  (interactive)
  (let ((word (thing-at-point 'word 'no-properties)))
	word))

(defun benching-at-point()
  ;;benching the function at point
  (interactive)
  (let* ((curr-func-name (get-word-on-point))
		 (bench-cmd (concat "go test -bench=" curr-func-name " .")))
	(shell-command bench-cmd)))

(defun benching-all ()
  ;;benching all the benchmark function in current file
  (interactive)
  (let ((bench-cmd (concat "go test -bench=. .")))
	(shell-command bench-cmd)))

(defun benching-golang ()
  "running and testing current file"
  (local-set-key (kbd "C-c C-c C-b") 'benching-at-point)
  (local-set-key (kbd "C-c C-c C-a") 'benching-all))

(add-hook 'go-mode-hook 'benching-golang)
``` 
now, when you are in a benching file buffer, move your point
to the benching function, then press"C-c C-c C-b", and wait
for the benching result to come. en!! fast and amazing!!!

## 3. how benchmarks work?
In the benching function, b.N start at 1. If the benched function completes in under 1 second--then b.N
is increase approximately 20% and benched function running again.

``` bash
goos: linux
goarch: amd64
BenchmarkFib20-8   	   46526	     25536 ns/op
BenchmarkFib28-8   	     939	   1204306 ns/op
PASS
ok  	_/home/hjiang/github/playground/golang/high_performance/benching	2.713s
``` 

In the benching result above, we can see almost 46000 loops took just over a second. and every loop took almost 25000ns.

## 4. how to increase benchmark accuracy?
Think the following scene, you have a function which took 0.5 second to finish.
when you do benching using the default benching time -- 1 second, the function 
only run two time. the average of this two runs may have a high standard deviation.

you can increase the benchmark time by the "-benchtime" flag:
``` bash
go test -bench . -benchtime=10s .
goos: linux
goarch: amd64
BenchmarkFib20-8   	  467419	     25576 ns/op
BenchmarkFib28-8   	    9517	   1200492 ns/op
PASS
ok  	_/home/hjiang/github/playground/golang/high_performance/benching	23.771s
``` 
    
also, you can do any number of benching for the same function, using "-count":
``` bash
go test -bench Fib20 -count=10 .
goos: linux
goarch: amd64
BenchmarkFib20-8   	   45270	     25574 ns/op
BenchmarkFib20-8   	   45276	     25659 ns/op
BenchmarkFib20-8   	   46740	     25609 ns/op
BenchmarkFib20-8   	   45705	     25610 ns/op
BenchmarkFib20-8   	   46526	     25597 ns/op
BenchmarkFib20-8   	   46834	     25701 ns/op
BenchmarkFib20-8   	   46414	     25543 ns/op
BenchmarkFib20-8   	   46909	     25561 ns/op
BenchmarkFib20-8   	   46792	     25571 ns/opp
BenchmarkFib20-8   	   46754	     25475 ns/op
PASS
ok  	_/home/hjiang/github/playground/golang/high_performance/benching	14.483s
``` 
you can see the ns/op changed vary little each running. our banching is reliable!
Also, you can use *benchstat* tool to tell how stable is your benching.
``` bash
go test -bench Fib20 -count=10 . >> old.txt
benchstat old.txt

name     time/op
Fib20-8  25.6µs ± 0%
``` 
the benching is very stable.
## 5. improve function performance, then do benching again
we hard code another number from the fibonacci series, reduce the depth of each recusive call by one.
``` go
func Fib3(n int) int {
	switch n {
	case 0:
		return 0
	case 1:
		return 1
	case 2:
		return 1
	default:
		return Fib2(n-1) + Fib2(n-2)
	}
}

func BenchmarkFib20(b *testing.B) {
	for n := 0; n < b.N; n++ {
		Fib3(20)
	}
}

func BenchmarkFib28(b *testing.B) {
	for n := 0; n < b.N; n++ {
		Fib3(28)
	}
}
``` 
compare the new version with the old version with *benchstat*:
``` bash
go test -bench Fib20 -count=10 . > new.txt

benchstat old.txt new.txt
name     old time/op  new time/op  delta
Fib20-8  39.2µs ± 0%  39.3µs ± 0%   ~     (p=0.424 n=10+10)

go test -bench Fib28 -count=10 . > new.txt
benchstat old.txt new.txt
name     old time/op  new time/op  delta
Fib28-8  1.84ms ± 0%  1.84ms ± 0%   ~     (p=0.870 n=10+10)
``` 
The recursion depth reducing does not have much effect on the
performance! We re-implement the fibonacci series generate function with iterate method.
``` go
func Fib_Iter(n int) int {
	a := 0
	b := 1
	c := 0

	if n == 0 {
		return a
	}

	if n == 1 {
		return b
	}

	for i := 2; i <= n; i++ {
		c = b + a
		a = b
		b = c
	}

	return c
}
``` 
Use benchstat do compare:
``` bash
benchstat old.txt new.txt

name     old time/op  new time/op  delta
Fib20-8  39.2µs ± 0%   0.0µs ± 1%  -99.97%  (p=0.000 n=9+10)
``` 
The iterate version is so much fast that the recur version.

## 6. avoid setup interference when benching
When need some setup before or in the middle of the benching,
one can reset the benching timer:

once per run setup, use ResetTimer():
``` go
func BenchmarkExpensive(b *testing.B) {
        boringAndExpensiveSetup()
        b.ResetTimer() 
        for n := 0; n < b.N; n++ {
                // function under test
        }
}
``` 

per loop setup, use StopTimer() and StartTimer():
``` go
func BenchmarkComplicated(b *testing.B) {
        for n := 0; n < b.N; n++ {
                b.StopTimer() 
                complicatedSetup()
                b.StartTimer() 
                // function under test
        }
}
``` 
## 7. miscellaneous in benching
### 7.1 record the number of memory allocations:
``` go
func BenchmarkRead(b *testing.B) {
        b.ReportAllocs()
        for n := 0; n < b.N; n++ {
                // function under test
        }
}
``` 
### 7.2 profiling benchmarks:
+ -cpuprofile=$FILE writes a CPU profile to $FILE.
+ -memprofile=$FILE, writes a memory profile to $FILE, -memprofilerate=N adjusts the profile rate to 1/N.
+ -blockprofile=$FILE, writes a block profile to $FILE.

**this the end of world now:) --> Do benching yourself!!!!**
