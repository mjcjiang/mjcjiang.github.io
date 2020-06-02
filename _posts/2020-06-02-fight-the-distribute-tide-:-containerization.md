# Containeriztion with Docker

When developing programs for a single mechine, the developing and running
environment is easy to control. But when comes to multi computers, things will
not be that easy. In this real world when i write this article, services are
running on lots of mechines distributed in different geographical locations.
How to manage these vast amount of modules, dependencies, mechines...? That\'s
the docker come to rescure.

## 1. concepts of Docker
Docker is a platform for developers and sysadmins to build, run, and share 
applications with containers. Containers have the following pros:

+ *Flexible*: Even the most complex applications can be containerized.
+ *Lightweight*: Containers leverage and share the host kernel, making them much 
  more efficient in terms of system resources than virtual machines.
+ *Portable*: You can build locally, deploy to the cloud, and run anywhere.
+ *Loosely coupled*: Containers are highly self sufficient and encapsulated, 
  allowing you to replace or upgrade one without disrupting others.
+ *Scalable*: You can increase and automatically distribute container replicas 
  across a datacenter.
+ *Secure*: Containers apply aggressive constraints and isolations to processes 
  without any configuration required on the part of the user.

### 1.1. Image in Docker

![Image](/assets/docker/Docker_Image.png)  
Fig 1. Image

An Image include everything needed to run an application - the code or binary,
runtimes, dependencies, and any other filesystem objects required.

### 1.2. Containers and VMs
![Containers_vs_VMs](/assets/docker/container_vs_vms.png)  
Fig 2. Containers vs Vms

A container has no differences with other process, it is just a process. But A VM
is a full-blown "guest" OS with virtual acess to host resources. So VMs are much
heavier than containers, they comsume much resources than containers.

### 1.3. make your hand dirty

After intall docker in your mechine, run the following command:
``` bash
$docker run hello-world
```

the running result:

``` bash
Unable to find image 'hello-world:latest' locally
latest: Pulling from library/hello-world
0e03bdcc26d7: Pull complete 
Digest: sha256:6a65f928fb91fcfbc963f7aa6d57c8eeb426ad9a20c7ee045538ef34847f44f1
Status: Downloaded newer image for hello-world:latest

Hello from Docker!
This message shows that your installation appears to be working correctly.

To generate this message, Docker took the following steps:
 1. The Docker client contacted the Docker daemon.
 2. The Docker daemon pulled the "hello-world" image from the Docker Hub.
    (amd64)
 3. The Docker daemon created a new container from that image which runs the
    executable that produces the output you are currently reading.
 4. The Docker daemon streamed that output to the Docker client, which sent it
    to your terminal.
......
```
The docker can\'t find a image which name is *hello-world* locally, so it pulls the 
image from remote Docker Hub.

## 2. Build and run image using docker
In this section, we build a image which running golang program. First, let\'s construct
the Dockerfile:
```
FROM golang:1.14

WORKDIR /go/src/app
COPY . .

RUN go install -v ./hello.go

CMD ["hello"]
```

+ FROM: golang:1.14 ; use the offical golang image as a parent image;
+ WORKDIR: all the following actions should be taken from the */go/src/app* in
  your *image file system*(not the host's file system)
+ COPY: copy all the file from Host to image.
+ RUN: run *go install* in the image file system
+ CMD: tell how to run an image

The code of go program:
```
package main

import "fmt"

func main() {
	fmt.Println("Hello, Welcome to golang docker version!")
}
```

Build out the image:
```
$sudo docker build -t hello-golang .
```

Build result:
```
Sending build context to Docker daemon  4.096kB
Step 1/5 : FROM golang:1.14
1.14: Pulling from library/golang
376057ac6fa1: Already exists 
5a63a0a859d8: Already exists 
496548a8c952: Already exists 
2adae3950d4d: Already exists 
039b991354af: Pull complete 
0cca3cbecb14: Pull complete 
59c34b3f33f3: Pull complete 
Digest: sha256:1e36f8e9ac49d5ee6d72e969382a698614551a59f4533d5d61590e3deeb543a7
Status: Downloaded newer image for golang:1.14
 ---> 7e5e8028e8ec
Step 2/5 : WORKDIR /go/src/app
 ---> Running in 118b8216c5fb
Removing intermediate container 118b8216c5fb
 ---> 4db1c79d2e68
Step 3/5 : COPY . .
 ---> b3637ab908d0
Step 4/5 : RUN go install
 ---> Running in 46d97f663f53
Removing intermediate container 46d97f663f53
 ---> ee0ccb10d3d5
Step 5/5 : CMD ["hello"]
 ---> Running in 9ac7876f482d
Removing intermediate container 9ac7876f482d
 ---> 68c71882b7bc
Successfully built 68c71882b7bc
Successfully tagged hello-golang:latest
```
Congratuation! You build out the image *hello-golang*. We test it by running:
```
$sudo docker run -it --rm --name my-running-app hello-golang
```

+ --rm: Automatically remove the container when it exits
+ --name: Assign a name to the container

## 3. Share your images
We you build out an image, you can share it with other people. The
following show how to share your images to Docker Hub.

+ Setup Docker Hub account
+ Tag your image
```
$ sudo docker tag hello-golang redmagic039/hello-golang
```
+ Login to Docker Hub
```
$ sudo docker login
```
+ Push image
```
$ sudo docker push redmagic039/hello-golang:1.0
```

Containeriztion and Images make a new program and deployment style.
Some great distributed builds can be builded on this basement.
