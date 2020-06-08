Typeclass In Haskell: Interface To Functional

Typeclasses allow you to define generic interfaces that provide a common feature 
set over a wide variety of types. In programming world, we often said that if 
something matches all the characters of a class, than this something belong to 
this class. "If it walks like a duck, drink like a duck, moo like a duck, then 
it is a duck." Typeclass is the duck of haskell.

# 1. Typeclass, why we need it
Image a something very bad: Haskell not implement the equality test *==*. You have
to implement it for every type you define. 

One time, you have a *Color* type, you define the *colorEq*:
```
data Color = Red | Green | Blue

colorEq :: Color -> Color -> Bool
colorEq Red   Red   = True
colorEq Green Green = True
colorEq Blue  Blue  = True
colorEq _     _     = False
```

Another time, you have a *String* Type([Char]), you define the *stringEq*:
```
stringEq :: [Char] -> [Char] -> Bool
-- Match if both are empty
stringEq [] [] = True
-- If both start with the same char, check the rest
stringEq (x:xs) (y:ys) = x == y && stringEq xs ys
-- Everything else doesn't match
stringEq _ _ = False
```

Now you can notice that, if define a *equalxx* for every type you define. This
will be a heavy and dull work!!! Why not just use *==* to compare anything? By
make *==* a generic function, we can make our code generic. If one piece of code
is used to compare things, then it can accept anythings that compiler know how
to compare. If new data types are add later, we just need asure the new types 
can be *compared*, so we do not have to change the code.

# 2. What are typeclasses?
Typeclasses define a set of functions that can have different implementations
depending on the type of data they are given. We define a typeclass:
```
class BasicEq a where
    isEqual :: a -> a -> Bool
```
This says that we are declaring a typeclass *BasicEq*, a is the instance types of
this typeclass. The instance type of this typeclass is any type that implements
the functions defined in the typeclass.

Let\'s look at defining isEqual for a particular type:
```
instance BasicEq Color where
    isEqual Red  Red  = True
    isEqual Green Green = True
    isEqual Blue Blue = True
    isEqual _     _     = False
```

load it into *GHCI*:
```
λ> isEqual Green Green
True
λ> isEqual Red Green
False
```

then define *isEqual* for another type:
```
instance BasicEq [Char] where
  isEqual [] [] = True
  isEqual (x:xs) (y:ys) = x == y && isEqual xs ys
  isEqual _ _ = False
```

load it into *GHCI*:
```
λ> isEqual "hjiang" "hjiang"
True
λ> isEqual "hjiang" "jiang"
False
```

Ok, let\'s dig more deep into typeclass. As everything has dark and bright
side. We want define a *Not Equal* function for our typeclass.

```
class BasicEq2 a where
  isEqual2 :: a -> a -> Bool
  isNotEqual2 :: a -> a -> Bool
```

Stop a while, think about it: if we know how two things equal each other, then
we already know how two things are not equal to each other. Vice versa, if we
know how two things are not equal, we already know how they equal each other.

```
class BasicEq3 a where
  isEqual3 :: a -> a -> Bool
  isEqual3 x y = not (isNotEqual3 x y)
  
  isNotEqual3 :: a -> a -> Bool
  isNotEqual3 x y = not (isEqual3 x y)
```

We only need to implement one function when define a *BasicEq3* instance type,
even though you can implement both of them.
```
instance BasicEq3 [Char] where 
  isEqual3 [] [] = True
  isEqual3 (x:xs) (y:ys) = x == y && isEqual3 xs ys
  isEqual3 _ _ = False
```

in the previous snippet, only implement the *isEqual2* function, then load it 
into ghci. 
```
λ> isNotEqual3 "hjiang" "heng"
True
λ> isNotEqual3 "hjiang" "hjiang"
False
```
Yeah! Though we not define *isNotEqual3*. The compiler consturct it for us!!

# 3. Some important typeclasses in Haskell
## 3.1 Show yourself, be yourself, leave your mask
The Show typeclass is used to convert values to Strings. The information of this
typeclass is:
```
type Show :: * -> Constraint
class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
  {-# MINIMAL showsPrec | show #-}
  	-- Defined in ‘GHC.Show’
......
```
the core function of this typeclass is a *show*, which convert some type value to
Strings. Let\'s *show* our *Color* in Haskell:
```
  instance Show Color where
  show Red = "[RED]"
  show Green = "[GREEN]"
  show Blue = "[BLUE]"
```
load it into GHCI:
```
λ> Green
[GREEN]
λ> Blue
[BLUE]
λ> Red
[RED]
```
Yeah, it really did what we tell to show.

## 3.2 Read, the reverse of a show
The typeclass *Read* is realy an introvert, when anyone else is showing and 
talking about themselves, he is just listening and reading. The information
of *Read* typeclass:
```
type Read :: * -> Constraint
class Read a where
  readsPrec :: Int -> ReadS a
  readList :: ReadS [a]
  GHC.Read.readPrec :: Text.ParserCombinators.ReadPrec.ReadPrec a
  GHC.Read.readListPrec :: Text.ParserCombinators.ReadPrec.ReadPrec
                             [a]
  {-# MINIMAL readsPrec | readPrec #-}
  	-- Defined in ‘GHC.Read’
```
the most important function:
```
read :: Read a => String -> a
```
some read example:
```
λ> read "10"
*** Exception: Prelude.read: no parse
λ> (read "10")::Integer
10
```

# 4. The End
Typeclass make our code generic and save our money and enegy very much,
so make your hand dirty in this classes!!! I just introduce some basic
knowledges.:)
