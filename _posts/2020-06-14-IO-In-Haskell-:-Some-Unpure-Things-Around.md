As a famous pure functional language, haskell make its functions very limitted.
In haskell a function can\'t change some state, like change the contents of a
variable. Call a function with the same parameter, function can nerver give you
back different results. Think about insert a node into a binary search tree in
haskell, the insert will result a new tree, not the old tree with the new node.

But if a function can\'t change anything in the world, how your program tell
some messages to you when it is running? Don\'t worry, haskell has seperates
the inpure functions from the pure ones. All the two systems contribute to 
our best service.

# 1. "Hello World" - your first real haskell program
Touch a file which name is *helloworld.hs*, content is:
```
main = putStrLn "Hello, World"
```
Compile and run the program:
```
ghc --make helloworld
...
./helloworld
```
Let\'s check the type of *putStrLn* in ghci:
```
λ> :t putStrLn
putStrLn :: String -> IO ()
λ> :t putStrLn "Hello, World"
putStrLn "Hello, World" :: IO ()
```
The ghci tell us that *putStrLn* takes a string and returns an I/O
action that has a result type of (). The I/O action is something that
carry out some side effect, like eat up some input or run out some
output.

So, you may ask when the IO action happen? This is why main comes in.
Give your *IO* action(s) a name main, then run the program.

You can also use *do* keyword to concatenate multipule *IO* actions
as \"one\" *IO* action:
```
main = do
  putStrLn "Who are you?"
  name <- getLine
  putStrLn ("Fine, thank " ++ name ++ " Bye!")
```
The privious do concat one *input I/O* and two *output I/O* as one.
The *do* will use the *I/O* result of the last *I/O* as its *I/O* result.

Let\'s inspect the more interesting *getLine*.
```
λ> :t getLine
getLine :: IO String
```
*getLine* is an I/O action that contains a result type String.
Which *name <- getLine* says is \"perform the I/O action getLine and
then binds its result value to name.\". The *<-* is just like a 
key to open the *I/O* box!!! In haskell, if we want to take the data
out of some I/O action, we must put this *take* action under some
I/O context. We must put this *inpure* action under the *inpure*
context.

With the *name* binded to the *I/O* result, we can make our
code *pure* again! Because the *name* is just a varible, can
can be used as parameter for any pure function.

```
nameTag = "Hello, my name is " ++ getLine  
```
The privious code is terribly wrong!!! You can\'t mix inpure I/O 
code with pure code!

## 1.1 return in I/O: this *return* is not that return
Let\'s see a program reverse your every input line:
```
main = do   
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
            main  
  
reverseWords :: String -> String  
reverseWords = unwords . map reverse . words 
```
The structure of this program: *if condition then I/O action else I/O 
action*. If you believe that *return* will terminate the whole program, 
then you are absolutely wrong. *return* in haskell I/O just encap some
value in the I/O action, it does not terminate the program.*return* is 
sort of the opposite to <-. While *return* takes a value and wraps it 
up in a box, <- takes a box (and performs it) and takes the value out 
of it, binding it to a name. 

## 1.2 Some useful functions when deal with I/O
+ putStr: like *putStrLn*, but do not print out new line;
+ putChar: print out a character;
+ print: just *putStrLn . show*, it will print out every instance of *Show*;
+ getChar: reads a character from the input;
+ when: a function in Control.Monad, take a boolean value and an I/O action.
        is boolean value is *True*, do the I/O action. If the value if *False*,
        just *return ()*.
+ sequnce: make a sequnce of I/O actions just like one I/O action, which this one 
           action is just like excute all these I/O actions in sequnce.
+ mapM: mapping a function that returns an I/O action over a list and 
then sequencing it.
  mapM_: just do not care the sequce of I/O result;
+ forever: repeat I/O action forever;
+ forM: make an I/O action for every element in this list. 
```
import Control.Monad  
      
main = do   
    colors <- forM [1,2,3,4] (\a -> do  
    putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors  
```

# 2. File and Streaming: Witness the lazy power of Haskell
## 2.1 Get your contents
Let\'s first see a cool function -- *getContents*, it reads everything from
the standard input until it encounters an end-of-file character. But the magic
bebind this function is it is a lazy I/O function, it will do the I/O action 
when you really need it.

```
main = do
  contents <- getContents
  let allLines = lines contents
      newcontents = unlines (map reverse allLines)
  putStr newcontents
```

The previous code read your input, and change every line in to its reverse.
```
~/github/playground/haskell $ ./io
The quick fox running to the box
xob eht ot gninnur xof kciuq ehT
with or with out you
uoy tuo htiw ro htiw
.......
```
Can you feel the lazy power now? when we knick out the *./io* into the shell,
then punch the *Enter* key, what the *contens* looks like? It is empty, nothing
in the contents variable can be used in the *do* body. But after we input our first
line, and punch the *enter* key, some thing amazing happens, the input line is
reversed! But What really amazing is in the next! The program does not finish!
It is waiting for the next input...

## 2.2 Get your contents 2.0: interact
A useful pattern when we do stream processing is "read --> process --> next read".
As an example, the code snippet in 2.1 just do "read a line --> reverse the line 
and put it out --> read the next line". Haskell give a *interact* function for 
this pattern. Let\'s rebuild the code:

```
main = interact reverseLine

reverseLine :: String -> String
reverseLine contents =
  let allLines = lines contents
      newcontents = unlines (map reverse allLines)
  in newcontents
```
First, let\'s check the type of *interact*:
```
λ> :t interact
interact :: (String -> String) -> IO ()
```
It receive a function as its parameter, which type is *String -> String*.
Becareful! The input *String* is all the contents of the input! you can
not pass the following function:
```
reverseLine :: String -> String
reverseLine line = reverse line
```
It just reverse a string not the full contents!

# 3. Read and Write file in haskell
## 3.1 read a contents of a file, the classic way
```
import System.IO  
      
main = do  
    handle <- openFile "girlfriend.txt" ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle  
```
The privious code snippet show a classic way of open and read a file:
+ import the *System.IO*, get all the toolset to deal with files;
+ openFile: which type is *FilePath -> IOMode -> IO Handle*, it eat
  a filepath with a given IOMode, do an IO action and return a *file 
  handler*;
+ hGetContents: get the contents from the *file handler*, just like
  *getContens* get contents from *stdio*;
+ process the contents, this code just print the contens out;
+ hClose: close *file handler*, let it go;

## 3.1 withFile, new type of reading:
We have a file records *Joseph Rudyard Kipling* poem *If*:
```
If you can dream—and not make dreams your master;
   If you can think—and not make thoughts your aim;
If you can meet with triumph and disaster
   And treat those two impostors just the same;
If you can bear to hear the truth you’ve spoken
   Twisted by knaves to make a trap for fools,
Or watch the things you gave your life to broken,
   And stoop and build ’em up with wornout tools;
```
and we want ccapitalize all words in this file. Now let do it:
```
import qualified Data.Char as Char
import System.IO

capitalizeWord :: String -> String
capitalizeWord (head:tail) = Char.toUpper head : map Char.toLower tail
capitalizeWord [] = []

capitalizeLine :: String -> String
capitalizeLine line =
  let allWords = words line
      newline = unwords (map capitalizeWord allWords)
  in newline

main = do
  withFile "if.txt" ReadMode (\handle -> do
                                 contents <- hGetContents handle
                                 let allLines = lines contents
                                     newContents = unlines (map (\line -> capitalizeLine line) allLines)
                                 putStr newContents)
```
We Compile and run our program:
```
$ cat if.txt | ./io 
If You Can Dream—and Not Make Dreams Your Master;
If You Can Think—and Not Make Thoughts Your Aim;
If You Can Meet With Triumph And Disaster
And Treat Those Two Impostors Just The Same;
If You Can Bear To Hear The Truth You’ve Spoken
Twisted By Knaves To Make A Trap For Fools,
Or Watch The Things You Gave Your Life To Broken,
And Stoop And Build ’em Up With Wornout Tools;
```
Yes, we did it!! Let\'s check the type of *withFile*:
```
λ> :t withFile
withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
```
FilePath and IOMode is our old friend, don\'t care about them.
We focus on the third parameter, a function which receive a file handler
and return a IO action. Finally, *withFile* use this IO action as
its result. This is our own *withFile* with a name *withFile\'*:
```
    withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a  
    withFile' path mode f = do  
        handle <- openFile path mode   
        result <- f handle  
        hClose handle  
        return result
```

# 4. Some useful function on file, your duty to research:
+ readFile:  readFile :: FilePath -> IO String
+ writeFile: writeFile :: FilePath -> String -> IO ()
+ appendFile : appendFile :: FilePath -> String -> IO ()

So, we talk too much on haskell I/O, it your time to do the I/O action!!!!
