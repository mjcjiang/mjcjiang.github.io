Recently I get a task to write a file processing tool which eats some files, do 
some processing and then output the result files. Some files are very small, just
about 100KB size, but some files are bigger than 4GB. To save processing time i 
let this tool process each file in a different thread, and use a monitor thread
to watch and tell the completeness of each processing.

# 1. multi-threading file processing
Use python *threading* module, play out a program, which process file in parallel:

``` python
import time
import os
import threading

processDict = {}

class MonitorThread(threading.Thread):
    def __init__(self, procdict):
        threading.Thread.__init__(self)
        self.procdict = procdict
    def run(self):
        while True:
            if len(self.procdict) == 0:
                return
            print('-' * 80)
            for k, v in self.procdict.items():
                print("file: %s:%%%d" % (k,v))
            print('-' * 80)
            time.sleep(2)
            os.system('cls' if os.name == 'nt' else 'clear')

class ReadWriteThread(threading.Thread):
    def __init__(self, threadID, inpath, outpath, procdict):
        threading.Thread.__init__(self)
        self.threadID = threadID
        self.inpath   = inpath
        self.outpath  = outpath
        self.procdict = procdict
    def run(self):
        #print("start process %s to %s..." % (self.inpath, self.outpath))
        processFile(self.inpath, self.outpath, self.procdict)
        #print("end process %s to %s..." % (self.inpath, self.outpath))
        
def processFile(inpath, outpath, procdict):
    procdict[inpath] = 0
    
    with open(inpath, 'r') as file, open(outpath, 'w') as out:
        file_size = os.stat(inpath).st_size

        while True:
            buf = file.read(15)
            if not buf:
                break
            out.write(buf)

            per = file.tell()/float(file_size)
            if int(100 * per) > procdict[inpath]:
                #print("[%s] Now process to %d percent!" % (inpath, int(100 * per)))
                procdict[inpath] = int(100 * per)

    del procdict[inpath]

fileProcThread1 = ReadWriteThread(1,'file1.txt', 'file1_out.txt', processDict)
fileProcThread2 = ReadWriteThread(2,'file2.txt', 'file2_out.txt', processDict)

moniThread = MonitorThread(processDict)

fileProcThread1.start()
fileProcThread2.start()
moniThread.start()
fileProcThread1.join()
fileProcThread2.join()
moniThread.join()
```
in each file process thread, when read out some block of file and process finish, update 
the completeness in *procdict* hash table.
```
procdict[inpath] = 0
......
per = file.tell()/float(file_size)
if int(100 * per) > procdict[inpath]:
#print("[%s] Now process to %d percent!" % (inpath, int(100 * per)))
    procdict[inpath] = int(100 * per)
```
the minitor thread check the hash table every 2 second:
```
for k, v in self.procdict.items():
    print("file: %s:%%%d" % (k,v))
    print('-' * 80)
    time.sleep(2)
    os.system('cls' if os.name == 'nt' else 'clear')
```

# 2. Use lock to protect completeness dict:
In the previous code, the completeness dict is accessing by multi-threads. 
Some write to it and some read from it. It is easy pron to *data-racing*
conditions! Use a lock to protect it:
```
procDictLock = threading.Lock()
...

procDictLock.acquire()
#use the dict...
procDictLock.release()
```
# 3. Finally
you can use monitor thread to what the tasks you most care about! Go on! Use it!

