Golang build concurrent programming into its blood by goroutine, which is its
killer attribute that attracts so many programmers and hackers. Now let\'s dig
into the "concurrent patterns" emerged from golang. First and the least, let\'s
see how to eliminate the "concurrent racing condition"!

# 1. Confinement by its human friend
Racing conditions happen when two or more (go :)routines visit the same piece of data
at the same time. These are the most familiar conditions when deal with concurrent programming.
Think about a scenario like this: when you will process 5 files each with its own 
goroutine, also you keep a hash table to keep each processness(finish its 50% processing, ...).
Within each file processing goroutine, it reports its processness into the hashtable
every 100ms. Outside all the goroutines, there is a seperate goroutine reports all
processness in the hash table every 100ms. We can use a graph to illustrate:

![concurrent files processing](/assets/goconcurr/file_process_concur.png)<br>

The 5 file processing goroutines and the report goroutine share the same hash table;
If we do not protect reading and writing this table carefully, race condition will happend,
and the whole program will crash without a trace!! 

We can manually make a law to seperate the sharing data, then reduce the race condition.
In the previous scenario, we can seperate the hash table into 5 small table, each 
goroutine has its own hash table. And the report process only read each hash table,
there is no write collisions between each file process routine.

![no-concurrent files processing](/assets/goconcurr/file_process_no_concur.png)<br>

In the privious scenario, we have define a *confinement*: each goroutine store and
process its own data(file and file processness). The *confinement* is just like a
law in human societies. From some point of views, it is more strict than human law,
because as a free will entry, we humans often break laws to do some breakthrough,
no matter for holy or evil.

# 2. Confinement examples when deal with goroutines
## 2.1 Ad-hoc confinement
The first example:

```
package main

import "fmt"

var data = make([]int, 4)

func main() {
	loopData := func(handleData chan<- int) {
		defer close(handleData)
		for i := range data {
			handleData <- data[i]
		}
	}

	handleData := make(chan int)
	go loopData(handleData)

	for num := range handleData {
		fmt.Println(num)
	}
}	
```
In this code snippet, *data* slice is only processed in *loopData* goroutine.
No other goroutines(include main routine) can come across with him. He is well
protected by his *mother* goroutine. This is where the most basic *confinement*
happens: *each block of data belong to its specific goroutine*.

But this confinement is so weak, if some new cowboy come to your team and
add other routines which access the *data*. The rule break and *race condition*
come back again! So we need more restrict rules.

## 2.2 Lexical confinement
```
package main

import "fmt"

func main() {
	chanOwner := func() <-chan int {
		results := make(chan int, 5)
		go func() {
			defer close(results)
			for i := 0; i <= 5; i++ {
				results <- i
			}
		}()
		return results
	}

	consumer := func(results <-chan int) {
		for result := range results {
			fmt.Printf("Received: %d\n", result)
		}
		fmt.Println("Done receive!")
	}

	results := chanOwner()
	consumer(results)
}
```
In this code snippet, *data* change to *results* channel.
And *results* defined in the lexical scope of chanOwner
function, no other goroutine can write into it.

Channel is concurrent safe by itself, now let\'s check 
some no-safe data structure:
```
printData := func(wg *sync.WaitGroup, data []byte) {
    defer wg.Done()

    var buff bytes.Buffer
    for _, b := range data {
			fmt.Fprintf(&buff, "%c", b)
		}
		fmt.Println(buff.String())
	}

var wg sync.WaitGroup
wg.Add(2)
data := []byte("golang")
go printData(&wg, data[:3])
go printData(&wg, data[3:])

wg.Wait()
```
In the code snippet, we split *data* into two sub block:
one is "gol", other is "ang". Each block blongs to a 
different routine.

  C(full) = A(part) + B(part);
  
  whatever you do in a routine has no effect on another.(also, you
can split data into k parts, and k routines deal with each part)

# 3. Conclusion
When we can find some confinement to eliminate racing condition
in concurrent programming, we need no sync primitives. The result
is good performance and easy code. But sometimes it is really 
difficult to establish confinement, and we need some other patterns
come to help!!
