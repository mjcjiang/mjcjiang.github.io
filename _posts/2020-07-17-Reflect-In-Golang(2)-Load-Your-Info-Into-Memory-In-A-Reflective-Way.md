In-memory data is the most retrived data in a system. When the system or applic
ation start, it always need load some data from disk to memory. When the file
ize is small and number of files is rare, it has not much problem. But in most 
context, the files are big and record in each file has different structure.
If we load each file in a specific process, we have to write one function for 
one file. It\'s a disaster when the number of files is big. In Go, we can use 
*reflect* to write a general file load function, what the user need is just:

+ the structure infomation of the record in the file
+ the path of the file in current mechine

Now, begin the hacking journey!

Tips:
+ all file in this article is text file
+ one line in file is a data record
+ record field is seperated by \| in file line

# 1. Util Function: Convert a string to a value of a specific type
When we read a line of a file, what we get is just a byte slice(or string).
This bytes of data is no meaning to us, if we can\'t transform it to the 
specific type of data we want. So we must write a transform function:
```
// convert a string to another type
func convString(s string, t reflect.Type) (interface{}, error) {
	//create a new element, which type is v
	velm := reflect.New(t).Elem()

	switch t.Kind() {
	case reflect.Int, reflect.Int8, reflect.Int16,
		reflect.Int32, reflect.Int64:

		//if s is an empty string, then set it to "0"
		if len(s) == 0 {
			s = "0"
		}

		if i, err := strconv.ParseInt(s, 10, 64); err != nil {
			return nil, err
		} else {
			velm.SetInt(i)
		}
		return velm.Interface(), nil
	case reflect.Uint, reflect.Uint16,
		reflect.Uint32, reflect.Uint64:

		//if s is an empty string, then set it to "0"
		if len(s) == 0 {
			s = "0"
		}

		if i, err := strconv.ParseUint(s, 10, 64); err != nil {
			return nil, err
		} else {
			velm.SetUint(i)
		}
		return velm.Interface(), nil
	//process load one character per field
	case reflect.Uint8:
		//if s is an empty string, then set it to "0"
		if len(s) == 0 {
			s = "\000"
		}

		sli := []uint8(s)
		velm.SetUint(uint64(sli[0]))
		return velm.Interface(), nil
	case reflect.String:
		velm.SetString(s)
		return velm.Interface(), nil
	default:
		return nil, fmt.Errorf("%s can not convert to type %v", s, t.String())
	}
}
```
Do unit test for this function:
```
v, err := convString("10", reflect.TypeOf(0))
	if err != nil {
		panic(err)
	}
	fmt.Printf("type: %T value:%v\n", v, v)

	v, err = convString("10.1", reflect.TypeOf(10.0))
	if err != nil {
		panic(err)
	}
	fmt.Printf("type: %T value:%v\n", v, v)

	v, err = convString("10.1", reflect.TypeOf(float32(10.0)))
	if err != nil {
		panic(err)
	}
	fmt.Printf("type: %T value:%v\n", v, v)

	v, err = convString("10.1", reflect.TypeOf("test"))
	if err != nil {
		panic(err)
	}
	fmt.Printf("type: %T value:%v\n", v, v)
```
print:
```
type: int value:10
type: float64 value:10.1
type: float32 value:10.1
type: string value:10.1
```
Yes! We get a specific value from a string according to the specific type!

Tips: you can change the function signature to (s []byte, t reflect.Type) ...

# 2. Util Function: Get the valid lines of a file
One line of a file is a data record, we can count the valid lines 
of the file. Then use this count and the record structure to pre-alloc
a slice of spaces.
```
// get the line count of a file
func lineCounter(r io.Reader) (int, error) {
	buf := make([]byte, 32*1024)
	count := 0
	lineSep := []byte{'\n'}

	for {
		c, err := r.Read(buf)
		count += bytes.Count(buf[:c], lineSep)

		switch {
		case err == io.EOF:
			return count, nil
		case err != nil:
			return count, err
		}
	}
}
```

# 3. The refLoad function
Read the file line by line, for each line, change this line
to a data record, and append to the finally result slice.
(We pre-alloc the slice, so it is not append, it just insert
it to a position in the slice).

```
// load reference data from file
func LoadRefDat(path string, sep string, s interface{}) (int, error) {
	index := 0

	//check if s is a pointer?
	v := reflect.ValueOf(s)
	if v.Kind() != reflect.Ptr {
		return 0, fmt.Errorf("non-pointer %v\n", v.Type())
	}

	//check if s point to a slice
	ve := v.Elem()
	if ve.Kind() != reflect.Slice {
		return 0, fmt.Errorf("pointed value non-slice %v\n", ve.Type())
	}

	//get the slice element type, and create a new element
	e := reflect.TypeOf(ve.Interface()).Elem()
	elem := reflect.New(e).Elem() //now elem can be setted
	numFields := elem.NumField()

	//read the file line by line
	file, err := os.Open(path)
	if err != nil {
		return 0, err
	}
	defer file.Close()

	linenum, err := lineCounter(file)
	if err != nil {
		return 0, err
	}

	//make slice, the size is linenum
	ve.Set(reflect.MakeSlice(ve.Type(), linenum, linenum))
	file.Seek(0, 0)

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		//split current line with separator
		trimline := strings.TrimSpace(scanner.Text())
		//if current line is empty and it is not the last line
		if len(trimline) == 0 && index < linenum-1 {
			continue
		}

		segs := strings.Split(trimline, sep)
		//line and slice type not match, and this line is not an empty line
		if len(segs) != numFields {
			return index + 1, fmt.Errorf("[line:%d] line seg num %d != struct field num %d", index+1, len(segs), numFields)
		}

		for i := 0; i < numFields; i++ {
			v, err := convString(strings.TrimSpace(segs[i]), elem.Field(i).Type())
			if err != nil {
				return index + 1, err
			}
			elem.Field(i).Set(reflect.ValueOf(v))
		}
		//append new elem to the slice
		ve.Index(index).Set(elem)
		index++
	}
	return index, nil
}
```
Tips: use scanner is not as effective as use byte slice. and 
strings.Split() will produce lots of small object in heap.
In the real environment, this could make a big burden to Go GC.
I just give a method, you can bring it to a better status.

# 4. Conclusion
Reflection in Go is a bit of confusing. But we can use it write
out some clean and generic module! Try your best to love it...:)
