Go’s select lets you wait on multiple channel operations. Combining goroutines 
and channels with select is a powerful feature of Go.

# 1. Basic Usage
```
ackage main

import (
    "fmt"
    "time"
)

func main() {
    c1 := make(chan string)
    c2 := make(chan string)

    go func() {
        time.Sleep(1 * time.Second)
        c1 <- "one"
    }()
    
    go func() {
        time.Sleep(2 * time.Second)
        c2 <- "two"
    }()

    for i := 0; i < 2; i++ {
        select {
        case msg1 := <-c1:
            fmt.Println("received", msg1)
        case msg2 := <-c2:
            fmt.Println("received", msg2)
        }
    }
}
```
In each goroutine, we use *time.Sleep* to stop some seconds. Just to 
simulate some compute-heavy tasks(such as RPC callings). When two goroutines
are created out, we *for-select* on the *c1* and *c2* channel, waiting
for something to happen!

After 1s passed, "one" will be writed to *C1*, "received one" will be printed.
After 2s passed, "two" will be writed to *C2*, "received two" will be printed.

```
received one
received two

Program exited.
```
 
# 2. Use channel to do variables iteration
We can use two channels and *for-select* to do variables iteration. Maybe
it's a litte confusing to tell what is *variables iteration*. We analogize
this to a familiar thing in our life: *water faucet*：

![glass water faucet](/assets/goconcurr/class_water_faucet.jpg)<br>

One channel serve as the water pipe, other serve as the tap.
```
package main

import (
	"fmt"
	"io/ioutil"
	"strings"
	"time"
)

func main() {
	done := make(chan int)

	bySlice, err := ioutil.ReadFile("./main.go")
	if err != nil {
		panic(err)
	}

	strSlice := strings.Fields(string(bySlice))

	strStream := strStreamGen(strSlice, done)

	i := 0
	for {
		if i > 20 {
			done <- 1
			break
		}

		i++
		fmt.Println(<-strStream)
		time.Sleep(time.Second)
	}
}

func strStreamGen(strSlice []string, done chan int) <-chan string {
	strStream := make(chan string)
	go func() {
		for _, s := range strSlice {
			select {
			case <-done:
				return
			case strStream <- s:
			}
		}
	}()

	return strStream
}
```
This code snippet read a file and split it into a words slice. Then pass
the slice into *strStreamGen* function, this function further create a
goroutine which *for-select* on two channels: one is working channel(water 
pipe), other is done channel(tap);

```
for _, s := range strSlice {
    select {
        case <-done:
            return
	    case strStream <- s:
    }
}
```
This *for-select* just tell us: if *done* channel is not filled, and *strStream*
channel is not blocked, push a word from the slice into *strStream* channel.

```
	i := 0
	for {
		if i > 20 {
			done <- 1
			break
		}

		i++
		fmt.Println(<-strStream)
		time.Sleep(time.Second)
	}
```
In the main routine, when 20 words piped out, close the tap by *done <- 1*. Runn
ing the code:
```
go run main.go

package
main
import
(
"fmt"
"io/ioutil"
"strings"
"time"
)
func
main()
{
done
:=
make(chan
int)
bySlice,
err
:=
ioutil.ReadFile("./main.go")
if
```
Just pipe out the first 20 word!!!

# 3. create goroutines infinitely waiting to be stopped
```
	done := make(chan int)

	go func() {
		for {
			select {
			case <-done:
				return
			default:
			}

			fmt.Println("Juming and Dancing!")
			time.Sleep(time.Second)
		}
	}()

	time.Sleep(time.Second * time.Duration(10))
	close(done)
```
The privious *for-select* use empty *default* tell: is *done* is not filled,
just pass the select and do something(this just print *jump and dancing*), 
and start the next for loop. This pattern is largely used in open-source
messaging systems(NSQD,NATS...).

In main, after 10 seconds, close done, and the pipe routine is returned.

# 4. Conclusion
*for-select* is a classic pattern in golang concurrent programming, it give
programmer the third eye to watch the channels they create. Use it and make
your life time much better.
