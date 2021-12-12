Struct can encapsulates data and action together, which is one of the main characters of OOP. Encapsulation provides three benifits:

1. The clients cannot directly modify the object's variables, one need inspect fewer statements to understand the possible values of those variables;
2. Hiding implementation details prevents clients from depending on things might change, which give designer the greater freedom to evolve the implememtation without breaking the API compatiblity.
3. It pevents clients from setting an object's variables arbitrarily. The variables can only be alters by method of the object, so the internal invariants can be maintained.

The following is a *IntSet* struct designed according to the *Encapsulation* method:
``` golang
// An IntSet is a set of small no-negative integers
// Its zero value represent the empty set
type IntSet struct {
	words []uint64
}

// Has report whether the set contains the no-negative value x
func (s *IntSet) Has(x int) bool {
	word, bit := x/64, x%64
	return word < len(s.words) && s.words[word]&(1<<bit) != 0
}

// Add adds the no-negative value x to the set
func (s *IntSet) Add(x int) {
	word, bit := x/64, x%64
	for word >= len(s.words) {
		s.words = append(s.words, 0)
	}
	s.words[word] |= (1 << bit)
}

// AddAll adds a sequces of no-negative values to the set
func (s *IntSet) AddAll(xs ...int) {
	for _, x := range xs {
		s.Add(x)
	}
}

// Remove removes the no-negative value x from the set
func (s *IntSet) Remove(x int) {
	word, bit := x/64, x%64
	if word >= len(s.words) {
		return
	}
	s.words[word] &= ^(1 << bit)
}

// UnionWith sets s to the union of s and t
func (s *IntSet) UnionWith(t *IntSet) {
	for i, tword := range t.words {
		if i < len(s.words) {
			s.words[i] |= tword
		} else {
			s.words = append(s.words, tword)
		}
	}
}

// IntersectWith sets s to the intersect of s and t
func (s *IntSet) IntersectWith(t *IntSet) {
	for i, tword := range t.words {
		if i < len(s.words) {
			s.words[i] &= tword
		}
	}
}

// DiffercenceWith set s to diffecence if s and t
func (s *IntSet) DifferenceWith(t *IntSet) {
	scopy := s.Copy()
	scopy.IntersectWith(t)
	for _, item := range scopy.Items() {
		s.Remove(item)
	}
}

// Items return set items as a slice
func (s *IntSet) Items() []int {
	var res []int
	for i, word := range s.words {
		if word == 0 {
			continue
		}

		for j := 0; j < 64; j++ {
			if word&(1<<uint(j)) != 0 {
				res = append(res, 64*i+j)
			}
		}
	}
	return res
}

// Len return the number of elements
func (s *IntSet) Len() int {
	res := 0
	for _, word := range s.words {
		if word == 0 {
			continue
		}

		for j := 0; j < 64; j++ {
			if word&(1<<uint(j)) != 0 {
				res += 1
			}
		}
	}
	return res
}

// Clear clear all elements from the set
func (s *IntSet) Clear() {
	for i := range s.words {
		s.words[i] &= 0
	}
}

// Copy return a copy of the set
func (s *IntSet) Copy() *IntSet {
	t := &IntSet{}

	t.UnionWith(s)
	return t
}

// String returns the set as a string of form "{1, 2, 3}"
func (s *IntSet) String() string {
	var buf bytes.Buffer
	buf.WriteByte('{')
	for i, word := range s.words {
		if word == 0 {
			continue
		}

		for j := 0; j < 64; j++ {
			if word&(1<<uint(j)) != 0 {
				if buf.Len() > len("{") {
					buf.WriteByte(' ')
				}
				fmt.Fprintf(&buf, "%d", 64*i+j)
			}
		}
	}
	buf.WriteByte('}')
	return buf.String()
}
```
Use struct and methods, you can implement *Encapsulation* as other languages. :)
