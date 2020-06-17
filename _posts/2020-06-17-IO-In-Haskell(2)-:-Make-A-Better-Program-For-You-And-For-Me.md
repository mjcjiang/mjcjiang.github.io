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
Tell us have finished what task?
The finished is finished any way! don't think about it anymore!
Start your life in the next way!
```
:), Speak in a sentence, it just delete the item you have finished!
Then we look back, find some drawbacks of the previous programs. Why 
we hardcode the *todo.txt* into the program? If we have another todo
list called "todo2.txt". (Too much work will kill you, take it easy, my friend!)
Why we seperate the *adding* and *delete* actions into two program? 
Stay safe! The holy *command line* will come home!!
