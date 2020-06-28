ByteString In Haskell: Let your big file fly high

A normal routine when your want to read and process file in haskell is like:
``` haskell
main = do
  handle <- openFile "todo.txt" ReadMode
  contents <- hGetContents handle
  ......(do process)
```
The *hGetContents* read the contents of the file, and store the result as a
haskell list which type is *[Char]*; As you already know, list evaluating in
haskell is very lazy(^_^ if you don't use a element, the element will not be
evaluated), so if your print one character a time in *contents*, the list will
be evaluated *n* times, which *n* equals to the number of characters in the file.
Use your toes to think, this is such a slow process when the file is bigger.

# Data.ByteString : the strict guy
The *Data.ByteString* eliminate the laziness completely, it just stands a series
of bytes in an array. A infinite strict bytestring? No such thing exists!!!
It means you load all the contents of file into memory, when the file is big,
this is always a problem.

# Data.ByteString.Lazy : the new kind of laziness
The *lazy* version ByteString read 64KB contents into memory each time. This
64KB ByteString is really a strict ByteString. But the contents of 64KB+ will
not be evaluated until you really use them. It just like some reading buffer.

# Compare *Normal File Process* and *Lazy File Process*
First, let\'s see how to reverse all lines in a file, using the normal method:
``` haskell
import System.Environment
import System.IO
import Data.List

main = do
  (fileName1:fileName2:_) <- getArgs
  revFileLines fileName1 fileName2
  
revFileLines :: FilePath -> FilePath -> IO ()
revFileLines source dest = do
  contents <- readFile source
  writeFile dest $ unlines $ map reverse $ lines contents
```
Compile and run the program, which eat a 1000000 lines file:
``` bash
$ghc -o NormalFileRead NormalFileRead.hs
$time ./NormalFileRead input.txt input_rev.txt
```
In my mechine, the time consumption:
``` bash
real	0m3.947s
user	0m3.644s
sys	    0m0.300s
```
It takes almost 4 seconds to read the file, reverse each line and write new lines out.

What about the lazy version? Check it out:
``` haskell
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as C

main = do
  (fileName1:fileName2:_) <- getArgs
  revFileLines fileName1 fileName2
  
revFileLines :: FilePath -> FilePath -> IO ()
revFileLines source dest = do
  contents <- C.readFile source
  let lines = C.lines contents
      revLines = map C.reverse lines
      newstr = C.unlines revLines
  C.writeFile dest newstr
```
The scketch of two program is same, but time consumption of the lazy version:
``` bash
real	0m1.454s
user	0m1.172s
sys	    0m0.281s
```
Just 1.45 seconds!!!

Do the same testing with 5000000 lines file:
```
NormalFileRead:
real	0m24.005s
user	0m21.856s
sys	    0m1.693s

ByteStringFileRead:
real	0m8.849s
user	0m7.133s
sys	    0m1.321s
```
Always fast!!!
