# Type in Haskell : Some good taste in your mouth
## 1. Taste the haskell static type system
Because haskell is a purely language, all computation are done via eval expressions
and yeild values. And haskell is a strong typed language, every value has an associated
type.Just like expressions denote values, type expressions denote *type values*(just types).

All haskell values are *first class*, but types are not. Types are used to describe
values, the association of a value with its type is called a typing. For example, check
the type of some value in GHCi:

![check type of some value in haskell](/assets/haskell/haskell_type_1.png)

\"::\" can be read as \"has this type\".

Haskell is a static typed system. If you mismatch types in some way. The 
haskell compiler will detect that, and the error will not flow
into the runtime level. For example, when you add two character using \'+\':

``` shell
<interactive>:746:1-9: error:
    • No instance for (Num Char) arising from a use of ‘+’
    • In the expression: 'a' + 'b'
      In an equation for ‘it’: it = 'a' + 'b'
``` 

the error message tell that \'+\' expect *Num* type parameter, but receive a *Char* type.

## 2. Haskell polymorphic types

Polymorphic type essentially describe families of types. For example, 
[a] describe a family of types, which spoke the sentence: \"Oh, this is 
a type family which contain *character list*([Char]), *number list*([Num]),
...... and what so ever list of the *single type* you can find in haskell.\"

A very good example, the following is a function to get the length of a haskell list:
``` haskell
getlen [] = 0
getlen (x:xs) = 1 + getlen xs
``` 

``` shell
> getlen [1,2,3]
3
> getlen []
0
``` 

now check the type of *getlen* function:
``` shell
>:t getlen
getlen :: Num p => [a] -> p
``` 

When we define the function, we do not explictly tell what the type this function
is; But after compile it, the compiler automatically inferred the type for us, let
read the type out: 

"*genlen* is a function of following type, give a list of any type, return a value 
of Num type"

Yes, *genlen* really eat a list and return the length of the list; In haskell, an
expression\'s or function\'s *principal type* is the general type that, intuitively
, \"contains all the instances of the expression\".

## 3. Define your own type in haskell
See the most simple type *Bool*, Bool type is defined as:

``` haskell
data Bool = False | True
``` 

\"Bool\" is type constructor and \"False\" and \"True\" are data constructor. We define
a new type *TrafficLight* represent a traffic light standing in a cross road:
``` haskell
data TrafficLight = Red | Yellow | Green
``` 

Like *Bool*, *TrafficLight* is enumerated type, since they consist of a finite
number of nullary constructor.

Here is a type with only one data constructor:
``` haskell
data Point a = Pt a a
``` 

*Point* now is a unary type constructor, from a orignal type a, it construct out
a new type *Point a*, let\'s check it out:

``` shell
λ> :t (Point "hjiang" "heng")
(Point "hjiang" "heng") :: Point [Char]

λ> :t (Point 10.0 11.0)
(Point 10.0 11.0) :: Fractional a => Point a
``` 

A very important distinction is that applying a data constructor to yeild a value and
applying a type constructor to yeild a type. data construct happen at running time, but
type construction happen at compile-time, ensuring the type safety.

### 3.1 define recursive type in haskell
Look at a simple list [1,2,3], it can be treat as 1:[2,3]; and [2,3] can be treat as
2:[3]; [3] can be seen as 3:[]. Finally, [1,2,3] is just 1:2:3:[]. This process of
list construction is a recursive process;

``` haskell
data MyList a = Empty | Cons a (MyList a)
``` 

Now let\'s implement the *hello-world* data structure in Haskell, binary tree:
``` haskell
type BiTree a = Empty | BiNode a (BiTree a) (BiTree a)
``` 

The BiTree type have two data constructor:
+ Empty, a nullary data constructor, construct an empty node, with nothing in it
+ Node, construct a node with some data and two sub trees, one left and one right;

Check the type of two data constructor:
``` shell
λ> :t Empty
Empty :: BiTree a
λ> :t BiNode
BiNode :: a -> BiTree a -> BiTree a -> BiTree a
``` 

BiNode is a function take an element of type \'a\' and Two Subtree with the type \'a\';
construct a new tree with type \'a\'.

## 4. List comprehensions 
As a Lisp dialects, lists are pervasive in Haskell, and more syntatic sugar to aid
in their creation. *list comprehension* is the most sweet one in these sugars.

``` haskell
λ> [ x + 1 | x <- [1,2,3]]
[2,3,4]
λ> [(x, y) | x <- [1,2], y <- [3,4]]
[(1,3),(1,4),(2,3),(2,4)]
λ> [x | x <- [3,1,2,10,11], x < 10]
[3,1,2]
λ> [ x + 1 | x <- [1,2,3]]
[2,3,4]
λ> [(x, y) | x <- [1,2], y <- [3,4]]
[(1,3),(1,4),(2,3),(2,4)]
λ> [x | x <- [3,1,2,10,11], x < 10]
[3,1,2]
``` 

We can use *list comprehension* to write a simple *quick sort*:
``` haskell
qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y < x]
               ++ [x]
               ++ qsort [y | y <- xs, y >= x]
``` 

Amazing, just four line of code!! Compare with your c/c++ coworkers!! Test it:

``` shell
λ> qsort [3,2,1,10]
[1,2,3,10]
λ> qsort [5,4,3,2,1]
[1,2,3,4,5]
``` 

Yes, it really work!!!

Haskell is a magic language for me! It deserves to be hacked!
