In-memory data is the most retrived data in a system. When the system or applic
ation start, it always need load some data from disk to memory. When the file
ize is small and number of files is rare, it has not much problem. But in most 
context, the files are big and record in each file has different structure.
If we load each file in a specific process, we have to write one function for 
one file. It's a disaster when the number of files is big. In Go, we can use 
*reflect* to write a general file load function, what the user need is just:

+ the structure infomation of the record in the file
+ the path of the file in current mechine

Now, begin the hacking journey!

Tips:
+ all file in this article is text file
+ one line in file is a data record
+ record field is seperated by '|' in file line

# 1. Util Function: Convert a string to a value of a specific type
When we read a line of a file, what we get is just a byte slice(or string).
This bytes of data is no meaning to us, if we can't transform it to the 
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
