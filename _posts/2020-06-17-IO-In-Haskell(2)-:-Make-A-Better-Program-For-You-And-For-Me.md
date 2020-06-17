Almost every program languange has its toolkit to deal with command line input,
haskell is not an exception. Now we dig in and see why we need it!

# 1. A disrepute stage show
Image you have a *todo list* on your table, every morning when you wake up, you write
all the tasks you need to finish in this beautiful day. And when you finish one task,
just delete the task from the list. Amazing, keep this habbit for a couple of years,
you will absolutly change your life!!!

First, coding the *adding* story line：
```
main = do
  todoItem <- getLine
  appendFile "todo.txt" (todoItem ++ "\n")
```
Ok! Compile the program, let\'s add some tasks of this day:
```
$ ./manufile
10:00 am, hacking the haskell programming
$ ./manufile
11:00 am, jumping and dancing
$ ./manufile
14:00 pm, singing a song that no one else sing
```
After we input the three tasks, check the *todo.txt* file:
```
$ cat todo.txt
10:00 am, hacking the haskell programming
11:00 am, jumping and dancing
14:00 pm, singing a song that no one else sing
```
OK! Finish the *adding* program. Next, it\'s time to design out the *delete* program:
```
main = do
  handle <- openFile "todo.txt" ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
  putStrLn "These are your TO-DO items: "
  putStr $ unlines numberedTasks
  putStrLn "Which one do you want to delete?"
  numberString <- getLine
  let number = read numberString
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile "todo.txt"
  renameFile tempName "todo.txt"
```
Holy shit!!! What a big mess! Don't worry, let\'s dig into the program line by line, i
translate the program into a small poem:
```
Open and handle your old todo list
Open an empty paper to record the new todo list
Shake your old one and let us see your life history
Tell us you have finished what task?
The finished was finished any way! don't think about it anymore!
Start your life in the next way!
```
:), Speak in a sentence, it just delete the item you have finished!

Then we look back, try to find some drawbacks of the previous programs. Why 
we hardcode the *todo.txt* into the program? If we have another todo
list called "todo2.txt". (Too much work will kill you, take it easy, my friend!)
Why we seperate the *adding* and *delete* actions into two program? 
Stay safe! The holy *command line* will come home!!

# 2. Haskell command line arguments
The *System.Environment* module has two cool I/O actions:
+ getArgs: IO [String], get the arguments that the program was run with and have
as its contained result a list with the arguments.
+ getProgName: Io String, a I/O action that contains the program name.

```
import System.IO
import System.Environment
import Data.List

main = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The arguments are: "
  putStrLn $ unlines (zipWith (\n arg -> show n ++ " : " ++ arg) [1..] args)
  putStrLn "The name of the program is: "
  putStrLn progName
```
Compile and run the program with arguments:
```
./manufile first second third
The arguments are: 
1 : first
2 : second
3 : third

The name of the program is: 
manufile
```
Wonderful(旺德福)！We get the arguments and progname! Now, make a salvage to
the bad behavior we made in the 1th section!
```
import System.Environment   
import System.Directory  
import System.IO  
import Data.List  
  
dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("add", add)  
            , ("view", view)  
            , ("remove", remove)  
            ]  
   
main = do  
    (command:args) <- getArgs  
    let (Just action) = lookup command dispatch  
    action args  
  
add :: [String] -> IO ()  
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")  
  
view :: [String] -> IO ()  
view [fileName] = do  
    contents <- readFile fileName  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks  
  
remove :: [String] -> IO ()  
remove [fileName, numberString] = do  
    handle <- openFile fileName ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let number = read numberString  
        todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! number) todoTasks  
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile fileName  
    renameFile tempName fileName
```
Compile and do some test:
```
$ ./todo view todo.txt
0 - 10:00 am coding to the next world
1 - 10:00 am, hacking the haskell programming
2 - 11:00 am, jumping and dancing
3 - 14:00 pm, singing a song that no one else sing

$ ./todo add todo.txt "15:30 pm, kick your ass"

$ ./todo view todo.txt
0 - 10:00 am coding to the next world
1 - 10:00 am, hacking the haskell programming
2 - 11:00 am, jumping and dancing
3 - 14:00 pm, singing a song that no one else sing
4 - 15:30 pm, kick your ass

$ ./todo remove todo.txt 0
$ ./todo view todo.txt
0 - 10:00 am, hacking the haskell programming
1 - 11:00 am, jumping and dancing
2 - 14:00 pm, singing a song that no one else sing
3 - 15:30 pm, kick your ass
```
First, we run *todo* program with *view* and *todo.txt*, it list out 
our todo tasks with its index. Then, add a todo task into the list.
Finally, delete the 0th task.

```
dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("add", add)  
            , ("view", view)  
            , ("remove", remove)  
            ]  
```
*dispatch* is a associate list, map a *string* to a I/O function.
Program check the first argument to find the action it will take.

# 3. Throw your dices in haskell: Randomness
What make haskell standing out is its pure functional character, this
promise that you can't get different results after calling a function
with the same parameter twices. But it can't deal with randomness, the
same function will return different results in different time.

Enter the *System.Random* module, it has all the functions that satisfy
our need for randomness. The first actor is *random* function:
```
Prelude System.Random> :t random
random :: (Random a, RandomGen g) => g -> (a, g)
```
+ RandomGen: typeclass is for types that can act as sources of randomness.
+ Random: typeclass is for things that can take on random values. A boolean
value can take on a random value, namely *True* and *False*. A number can 
also take on a random value. We can translate the signature of *random* as
following: *it takes a random generator and return a random value and a new random
generator*.

```
Prelude System.Random> random (mkStdGen 1000)
(1611434616111168504,1261958764 2103410263)
Prelude System.Random> random (mkStdGen 1000) :: (Integer, StdGen)
(1611434616111168504,1261958764 2103410263)
Prelude System.Random> random (mkStdGen 1000) :: (Bool, StdGen)
(True,40054014 40692)
Prelude System.Random> random (mkStdGen 1000) :: (Bool, StdGen)
(True,40054014 40692)
Prelude System.Random> random (mkStdGen 1000) :: (Float, StdGen)
(0.8345514,698578198 1655838864)
```
*StdGen* is an instance of RandomGen class, *mkStdGen* takes a integer and 
return an random generator.

Let\'s see a real world example, throw a dice three times:
```
threeCoins :: StdGen -> (Bool, Bool, Bool)  
threeCoins gen =   
    let (firstCoin, newGen) = random gen  
        (secondCoin, newGen') = random newGen  
        (thirdCoin, newGen'') = random newGen'  
    in  (firstCoin, secondCoin, thirdCoin)
```
See the newGen, newGen' and newGen'', A ha! This why *random* return 
a random value and a new Randomness Generator!!! We use *stack* build
a real example:

```
$ stack new random-project
```
The tree of the *random project*:

```
.
├── app
│   └── Main.hs
├── ChangeLog.md
├── LICENSE
├── package.yaml
├── random-project.cabal
├── README.md
├── Setup.hs
├── src
│   └── Lib.hs
├── stack.yaml
├── stack.yaml.lock
└── test
    └── Spec.hs
```

We define our coin function in Lib.hs:
```
module Lib
    ( someFunc,
      randomTest
    ) where

import System.Random

someFunc :: IO ()
someFunc = putStrLn "someFunc"

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, newGen'') = random newGen'
  in (firstCoin, secondCoin, thirdCoin)

randomTest :: Int -> IO ()
randomTest seed = do
  let (first, second, third) = threeCoins (mkStdGen seed)
  putStrLn (show first)
  putStrLn (show second)
  putStrLn (show third)
```
In *Main.hs* we read the command line argument, and execute coin
throwing:
```
module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  (seed:xs) <- getArgs
  randomTest (read seed)
```
Build and run with different arguments:
```
$ stack exec random-project-exe 100
True
False
False

$ stack exec random-project-exe 101
True
True
True

$ stack exec random-project-exe 102
True
True
True
```
We just use the command line toolkit in the 2th section!!!
