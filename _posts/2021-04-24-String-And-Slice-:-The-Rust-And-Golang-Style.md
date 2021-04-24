Array, String and Slice is the basic element of a program in modern high-order
programming language, but how different language deal with them? We will inspect
the Golang and Rust way.

# 1. Array, the same kind of things in a sequence
Array have the following attribute:
+ the size is know in compile time
+ the type of all elements are the same
+ you can not change the elements by default[Rust]

```
	a := [3]int{1, 2, 3}
	fmt.Println(a)
	a[1] = 4
	fmt.Println(a)
```
running result:
![interface var](/assets/array/golang_array.png)<br>
the array in golang can be changed
