Reflection is a kind of metaprogramming. With the help of reflection,
the program can examine its own structure. In this article we first 
clarify how reflection work in Go, then build a file load program for
action.

# 1. Types and Interface in Go

## 1.1 types in Go

Go is a statically typed lauguage, one variable only has one known 
type at any compile time. See the following example:
```
package main

type MyInt int

func main() {
	var foo int = 10
	var bar MyInt = foo
}
```
run this program:
```
./test_reflect.go:7:6: cannot use foo (type int) as type MyInt in assignment
```
Though the underline type of *foo* and *bar* is same(int), but their static
type is different. They can not be assained to one another without conversion.

Let's see the most important and great type in golang: Interface types. An 
Interface variable can store any concrete value as that value implements the
interface's methods.
```
// Reader is the interface that wraps the basic Read method.
type Reader interface {
    Read(p []byte) (n int, err error)
}

// Writer is the interface that wraps the basic Write method.
type Writer interface {
    Write(p []byte) (n int, err error)
}
```
*Reader* is an interface which has only one method: *read*. Anything that
implements the method can be seen as a Reader, can be used as a Reader.
```
var r io.Reader
r = os.Stdin
r = bufio.NewReader(r)
r = new(bytes.Buffer)
// and so on
```
A variable of type *io.Reader* can hold any value whose type has a *Read*
method. An extremely important example of an interface type is the empty
interface:
```
interface{}
```
it is satisfied by all values at all. And Reflections and Interfaces are
close related.

## 1.2 the representation of an interface
What is the internal data structure of an interface variable? 

![interface var](/assets/reflect/structure_of_interface_var.png)<br>

The interface variable contains a pair:
+ the concrete variable(which implement the interface)
+ the type info of the concrete variable(full information)

```
var r io.Reader
tty, err := os.OpenFile("/dev/tty", os.O_RDWR, 0)
if err != nil {
    return nil, err
}
r = tty
```
r is just pair (tty, *os.File), now we can do:
```
var w io.Writer
w = r.(io.Writer)
```
*r.(io.Writer)* is a *type assertion*;
```
var empty interface{}
empty = w
```
the *empty* interface variable contain all the type information of
the concrete value, but it can not access any mothod of this value.
(it is empty:))


# 2. The laws of reflection

## 2.1 interface value to reflect object:

Go's reflect package has tool to examine the (value, type) pair
stored in a interface variable.
+ reflect.Type -- get by reflect.TypeOf()
+ reflect.Value -- get by reflect.ValueOf()

```
var x float64 = 3.14
fmt.Println("type: ", reflect.TypeOf(x))
```
the program prints:
```
type:  float64
```
The signature of reflect.Typeof:
```
// TypeOf returns the reflection Type of the value in the interface{}.
func TypeOf(i interface{}) Type
```
When we call reflect.TypeOf(x), x is first stored in an empty interface, 
which is then passed as the argument; reflect.TypeOf unpacks that empty 
interface to recover the type information.

```
fmt.Println("value: ", reflect.ValueOf(x))
fmt.Println("value: ", reflect.ValueOf(x).String())
```
result:
```
value:  3.14
value:  <float64 Value>
```
other useful method of reflect.Type and reflect.Value:
```
var x float64 = 3.4
v := reflect.ValueOf(x)
fmt.Println("type:", v.Type())
fmt.Println("kind is float64:", v.Kind() == reflect.Float64)
fmt.Println("value:", v.Float())
```
print:
```
type: float64
kind is float64: true
value: 3.4
```

## 2.2 reflection object to interface value
Given a reflect.Value we can recover an interface value using the Interface method; 
in effect the method packs the type and value information back into an interface
representation and returns the result:

```
// Interface returns v's value as an interface{}.
func (v Value) Interface() interface{}
```

Interface() method is the reverse of ValueOf()!!


## 2.3 to modify a reflection object, the value must be settable
```
var x float64 = 3.4
v := reflect.ValueOf(x)
v.SetFloat(7.1)
```
run the previous code, you will get:
```
panic: reflect: reflect.flag.mustBeAssignable using unaddressable value
```
The problem is not that the value 7.1 is not addressable; it's that v is not settable. 
Settability is a property of a reflection Value, and not all reflection Values have it.

we can use tool test if a reflect.Value can be setted:
```
var x float64 = 3.4
v := reflect.ValueOf(x)
fmt.Println("v can be setted?: ", v.CanSet())
```
prints:
```
v can be setted?:  false
```
We called Set method on a non-settable Value! What is settability??
Ans: When we use *reflect.ValueOf(x)* create a interface variable,
the value it contains is not the real *x*, but a copy of *x*. So, 
your "Set" on this fake value have no effect on the real one, it is
just a waste, so Go just refuse this kind of usage!

Just reflect on the address of the variable:
```
var x float64 = 3.4
p := reflect.ValueOf(&x)
fmt.Println("p's type: ", p.Type())
fmt.Println("p's kind: ", p.Kind())
fmt.Println("p can be setted?: ", p.CanSet())
```
print:
```
p's type:  *float64
p's kind:  ptr
p can be setted?:  false
```
the reflect object v can't be settable, not the value it contains:
```
v := p.Elem()
fmt.Println("v can be setted?: ", v.CanSet())
```
print:
```
v can be setted?:  true
```
now we set it:
```
v.SetFloat(7.1)
fmt.Println(x)
```
print:
```
7.1
```

# 3. reflect and struct

As long as we can get the address of a structure, we can modify
its fields.

```
type T struct {
		A int
		B string
}

t := T{23, "skidoo"}
s := reflect.ValueOf(&t).Elem()
typeOfT := s.Type()

for i := 0; i < s.NumField(); i++ {
    f := s.Field(i)
    fmt.Printf("%d: %s %s = %v\n", i,
    typeOfT.Field(i).Name, f.Type(), f.Interface())
}
```
print: 
```
0: A int = 23
1: B string = skidoo
```
Tips: The field name of T must upper case, then it can be settable.
```
s.Field[0].SetInt(77)
s.Field[0].SetString("Test")
fmt.Println("t is now", t)
```
print:
```
t is now {77 Test}
```

# 4. Reflection and Slice

Let\'s see a simple example, use reflection traverse two different type of
slices.
```
package main

import (
	"fmt"
	"reflect"
)

func main() {
	strArr := []string{"jim", "marry", "alon"}
	testSliceReflect(strArr)
	intArr := []int{1, 3, 2, 4}
	testSliceReflect(intArr)
}

func testSliceReflect(t interface{}) {
	switch reflect.TypeOf(t).Kind() {
	case reflect.Slice:
		s := reflect.ValueOf(t)

		for i := 0; i < s.Len(); i++ {
			fmt.Println(s.Index(i))
		}
	}
}
```
First, use reflect.TypeOf(t).Kind() get the kind of object
contained in the interface value. If it is a slice, no matter
the type of elements in the slice, just print it out.

Now, we begin to see a complicated example: pow a int slice
```
package main

import (
	"fmt"
	"reflect"
)

func main() {
	foo := []int{1, 2, 3, 4, 5, 6}
	testSliceReflectWrite(&foo)
	fmt.Println(foo)
}

func intPow(i int) int {
	return i * i
}

func testSliceReflectWrite(t interface{}) {
	if reflect.TypeOf(t).Kind() != reflect.Ptr {
		fmt.Println("parameter not a ptr")
	}

	s := reflect.ValueOf(t).Elem()
	etype := reflect.TypeOf(s.Interface()).Elem()
	tmp := reflect.New(etype).Elem()

	for i := 0; i < s.Len(); i++ {
		pow := intPow(int(s.Index(i).Int()))
		tmp.Set(reflect.ValueOf(pow))
		s.Index(i).Set(tmp)
	}
}
```
We pass a pointer to slice to the *testSliceReflectWrite*, so
we can assign value to the element of the slice.

First, *reflect.ValueOf(t)* get the value of t:
```
fmt.Printf("ps: %v\n", reflect.ValueOf(t))
```
print:
```
ps: &[1 2 3 4 5 6]
```
the value in the interface variable t is a slice pointer

Next, *reflect.ValueOf(s).Elem()* get the value the pointer point to.
Yes, it is the slice.
```
fmt.Printf("s: %v\n", s)
```
print:
```
s: [1 2 3 4 5 6]
```

Now, we want get the the type of the slice element, so we can creat a
temp variable according to this *standard type*. Because we have the 
reflect.Value type of s, we use Interface() get back the type info,
then TypeOf() get type info. 
```
fmt.Println(reflect.TypeOf(s.Interface()))
```
print:
```
[]int
```
Yes, it is a int slice, *Elem()* on this reflect.Type:
```
etype := reflect.TypeOf(s.Interface()).Elem()
fmt.Println(etype)
```
print:
```
int
```
reflect.New(etype) return a ptr point to value of this type(int)
so Elem() get the value; finally the for loop do the pow setting.
run the whole program:
```
before:  [1 2 3 4 5 6]
after:  [1 4 9 16 25 36]
```

Just do a slice pow, it\'s an overkilling to use reflect. But this 
is a good example tell us how we can use reflect to get and set our
own data.
