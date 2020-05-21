<br>
<br>
<p>分布式系统构建有一个重要指标，就是如何让消息在系统模块之间高效且可靠的传输。消息中间件
即是这样一类在分布式系统中支持消息收发的软硬件基础设施。可以支持分布在不同操作系统的模块，
使用不同的网络协议进行消息通信；并为程序开发人员屏蔽操作系统和网络通信的细节。</p>  
  
  本文以RabbitMQ为核心构件，设计并实现一个支持可靠消息通信的节点集群；并在此基础上统计集
群的最大消息吞吐量和消息平均延迟。本文主要就以下几个方面展开描述：  
..* RabbitMQ简介  
..* RabbitMQ常用消息传输模式  
..* RabbitMQ构件集群实践，以及对集群如何实现高可用展开描述  
..* 集群性能测试  
..* 结论  
  
# 1. What is RabbitMQ?
RabbitMQ是一个开源的消息消息代理(message-broker)软件，起初支持AMQP(Advanced Message
Queuing Protocol)协议。后续版本支持插件模式，增加了对STOMP,MQTT等其他协议的支持。RabbitMQ
用erlang作为其主要开发语言，并用erlang的OTP框架实现集群化操作，并对节点错误进行有效处理。
RabbitMQ现在已经实现对大部分开发语言的客户端支持。
  
# 2. RabbitMQ常用消息传输模式
## 2.1 最简单的Producer-Consumer机制
       
![producer-consumer messaging](/assets/rabbitcluster/pc.png)<br>
   fig.1 procuder-consumer messaging  
   P(生产者)发送数据到RabbitMQ的一个队列上，C(消费者)在这个队列上等待消息的到来
  
## 2.2 Work Queues机制
  
![work-queue](/assets/rabbitcluster/work_queue.png)<br>
   fig.2 work-queue messaging
   
   P(生产者)发送数据到RabbitMQ的一个队列上，多个消费者(C1,C2)共享这个队列中的消息。
   队列中的消息以Round-Robin的方式分别被POP给C1,C2;
  
## 2.3 Pub-Sub机制
  
![pub-sub](/assets/rabbitcluster/pub_sub.png)<br>
   fig.3 publish-subcribe messaging  

   P(生产者)不再直接发送数据到queue上，而是将数据发送到一个exchange上；这个exchange
   会根据一个预先设定的策略转发消息到对应的队列上(甚至丢弃一些消息）。 
  
## 2.4 Routing机制
  
![routing](/assets/rabbitcluster/routing.png)<br>
  
   fig.4 routing messaging
   
   在使用Routing机制进行消息传输的时候，exchange的类型应设为direct(定向);Publisher发送 
   的每一个消息中都会带上一个Routine-Key,Queue和exchange之间进行绑定时，会注册对应的Routine-Key; 
   如上图所示，Q2注册了black和green作为Routine-Key,所有Routine-Key为black或green的消息 
   都会被发送到Q2队列。 
  
# 2.5 Topics机制

![topics](/assets/rabbitcluster/topics.png)<br>
  
   fig.5 topics messaging 
  
   在使用Topics机制进行消息传输的时候，exchange的类型应设为topic(主题);Queue通过配符的方式和
   exchange之间绑定Routine-Key;通配符的意义  
   ..* \* : 代替一个单词   
   ..* \# : 代替0个或多个单词  
   如上图所示，*.orange.*表示Q1会接受任何“三个word,并且中间一个为orange”的Routine-Key的消息；
   如"lazay.orange.fox".
  
# 3. High Availiable RabbitMQ Cluster构建
## 3.1 rabbitmq集群通过queue mirroring实现高可用原理
  
![morroring queue](/assets/rabbitcluster/queue_mirror.png)<br>
   Fig.6 queue mirroring  
  
   一笔消息进入RabbitMQ集群后，会先被写入到master queue中，同时每个slave queue开始
   对这笔消息进行同步，只有当所有slave queues完成同步之后，集群才会给客户端返回消息
   响应。客户端未收到当前的消息响应之前，不会发送下一笔消息。这样就保证了集群中所有
   节点上queue的消息内容和顺序完全相同。同时，当master queue所在节点failover时，任何
   一个同步完成的queue都可以升级为master queue.
  
   利用上述原理，还可以实现多主题消息定序。以交易系统最常见的买卖订单为例，假定目前
   存在两个主题order.sell和order.buy。可以使用2.5中的topics机制，在exchange和queue
   之间用order.*作为Routine-Key进行绑定。所有以order.sell和order.buy为routine-key
   的消息都会被发送到这个队列上。下面是两笔消息的时序图:
  
![two orders arrive in a rabbitmq cluster](/assets/rabbitcluster/two_orders.png)<br>
   Fig.7 two orders enter rabbitmq cluster
     
   如上图所示，两笔消息的顺序在master queue和各slave queue上始终是一致的。
  
## 3.2 集群构建实践
   本文中的样例搭建在LAN中的三台主机上，主机信息如下：<br>  
   | Linux hjiang-HP 5.3.0-18-generic #19-Ubuntu SMP Tue Oct 8 20:14:06 UTC 2019 x86_64 x86_64 x86_64 GNU/                |<br>  
   | Linux xiufuzhang 5.3.0-51-generic #44~18.04.2-Ubuntu SMP Thu Apr 23 14:27:18 UTC 2020 x86_64 x86_64 x86_64 GNU/Linux |<br>  
   | Linux nsxia 4.15.0-91-generic #92-Ubuntu SMP Fri Feb 28 11:09:48 UTC 2020 x86_64 x86_64 x86_64 GNU/Linux             |<br>  
  
   三台主机上运行的rabbitmq版本保持一致：  
  
![rabbit version](/assets/rabbitcluster/rabbit_version.png)<br>
   Fig.8 rabbitmq version
  
   集群的构建过程请参见官方文档：https://www.rabbitmq.com/clustering.html ;这里不做赘述。
  
   集群构建完成后，在任意一台主机上察看集群情况：  
   $rabbitmqctl cluster_status  
  
![rabbit cluster status](/assets/rabbitcluster/rabbit_cluster_status.png)<br>
   Fig.9 rabbitmq cluster status   
     
   同时每台主机在http://localhost:15672 上开放HTTP管理界面，如下：  
![http monitor](/assets/rabbitcluster/rabbit_http_monitor.png)<br>
   Fig. 10 http monitor page  
  
# 4. 性能测试
<p>  
  在集群上添加mirrored queues, 信息如下：  
</p>

![mirrored queues](/assets/rabbitcluster/mirror_queues.png)<br>
  Fig.11 add a mirrored queue  
<p>
  ha-params = 3:             一个master queue, 两个slave queuue;  
  ha-sync-mode = automatic:  slave queue自动同步master queue;  
  state = idle:              当前queues处于闲置状态，没有数据被写入  
</p>  
  
## 场景1: 只有Produer, 没有Consumer, 写入10w条1k大小消息：
<p>  
发送方程序：  
</p>  
  
``` go
package main  
  
import (  
	"fmt"  
	"log"  
	"os"  
	"strconv"  
	"time"  
  
	"github.com/streadway/amqp"  
)  
  
func main() {  
	msgnum, err := strconv.Atoi(os.Args[1])  
	if err != nil {  
		fmt.Println(err)  
		os.Exit(1)  
	}  
  
	//dial rabbitmq server  
	conn, err := amqp.Dial("amqp://guest:guest@localhost:5672")  
	failOnError(err, "Failed to connect to RabbitMq")  
	defer conn.Close()  
  
	//create a channel, which encapsulates most APIs get things done  
	ch, err := conn.Channel()  
	failOnError(err, "Failed to create channel")  
	defer ch.Close()  
  
	msgBody := make([]byte, 1024)  
	copy(msgBody, []byte("hello world"))  
  
	start := time.Now()  
	for i := 1; i <= msgnum; i++ {  
		err = ch.Publish(  
			"",               //exchange  
			"mirrored.queue", //routine key  
			false,            //mandatory  
			false,            //immediate  
			amqp.Publishing{  
				ContentType: "text/plain",  
				Body:        []byte(msgBody),  
			})  
		failOnError(err, "Failed to publish a message")  
	}  
	elapse := time.Now().Sub(start)  
	fmt.Printf("elapsed time: %v, send %v messages!\n", elapse, msgnum)  
}  
  
func failOnError(err error, msg string) {  
	if err != nil {  
		log.Fatalf("%s: %s", msg, err)  
	}  
}  
``` 
<p>  
   运行4次测试程序，每次都发送10w条1k的消息：  
  
![bench 1w message in 1k size](/assets/rabbitcluster/bench_10k_1.png)<br>
   Fig.12 benchmark 1  
  
   在场景1下，可以实现 4w笔/秒 的消息写入。  
</p>  
  
## 场景2: 单Producer, 单Consumer, 写入10w条1k大小消息：
<p>  
接收方程序：  
</p>  
  
``` go
package main  
  
import (  
	"log"  
  
	"github.com/streadway/amqp"  
)  
  
func main() {  
	//dial rabbitmq server  
	conn, err := amqp.Dial("amqp://guest:guest@localhost:5672")  
	failOnError(err, "Failed to connect to RabbitMq")  
	defer conn.Close()  
  
	//create a channel  
	ch, err := conn.Channel()  
	failOnError(err, "Failed to create channel")  
	defer ch.Close()  
  
	msgs, err := ch.Consume(  
		"mirrored.queue",  
		"",  
		true,  //auto ack  
		false, //exclusive  
		false, //no-local  
		false, //no-wait  
		nil,   //args  
	)  
	failOnError(err, "Can not register a consumer")  
  
	forever := make(chan bool)  
  
	go func() {  
		for _ = range msgs {  
			//log.Printf("Recived a message: %s\n", d.Body)  
		}  
	}()  
  
	log.Printf(" [*]Waiting for message, To exit press Ctrl+C")  
	<-forever  
}  
  
func failOnError(err error, msg string) {  
	if err != nil {  
		log.Fatalf("%s: %s", msg, err)  
	}  
}  
``` 
   运行4次测试程序，每次都发送10w条1k的消息：  
  
![context 2](/assets/rabbitcluster/one_p_one_c.png)<br>
   Fig.13 benchmark 2  
  
<p>  
   在场景2下，写入吞吐量上升到 7w笔/秒。  
</p>  
  
## 场景3: 单Producer, 单Consumer, 写入10w条1k大小消息，计算消息平均时延：
<p>  
 发送方代码，将发送时间戳带入消息体：  
</p>  
  
``` go
start := time.Now()  
for i := 1; i <= msgnum; i++ {  
    //fmt.Println(time.Now().UnixNano())  
	stamp := strconv.FormatInt(time.Now().UnixNano(), 10)  
	//fmt.Println(stamp)  
	//copy(msgBody, []byte(stamp))  
	  
	err = ch.Publish(  
	    "",               //exchange  
		"mirrored.queue", //routine key  
		false,            //mandatory  
		false,            //immediate  
		amqp.Publishing{  
		    ContentType: "text/plain",  
			Body:        []byte(stamp),  
		})  
		  
	failOnError(err, "Failed to publish a message")  
	time.Sleep(time.Millisecond * 1)  
}  
``` 
<p>  
 接收方代码，提取消息体中的时间戳，计算和当前时间戳的差值，即消息时延。  
</p>  
  
``` go
go func() {  
    var sum int64 = 0  
	var i int64 = 1  
	for d := range msgs {  
	    n := time.Now().UnixNano()  
		s, _ := strconv.ParseInt(string(d.Body), 10, 64)  
		dis := n - s  
		sum += dis  
		fmt.Println(sum / i)  
		i++  
	}  
}()  
``` 
<p>  
  最终测得10笔消息的平均时间延迟为300us左右。  
  tips: 发送方间隔1ms发送一笔消息，否则rabbitmq会因为负载过大导致延迟巨增！  
</p>  
  
# 5. 结论
<p>  
  本文实现了一个在LAN集群中部署RabbitMQ高可用消息传输集群的方案；实现消息的多主机备份，  
单主题和多主题的消息定序等高可用核心功能；在1000Mb/s的局域网中可以实现4w/s以上,1Kb大小的  
消息传输，消息传输时延控制在300us左右；在万兆网上的传输时间延可以更低。  
</p>  
