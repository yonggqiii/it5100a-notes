![Updated][update-shield]

In software engineering, the need to perform multiple tasks simultaneously gives rise to two (not mutually exclusive) approaches: concurrency and parallelism. For example, game servers are not monolithic entities; rather, a game server comprises many components, each interacting with the external world. One component might be dedicated to managing user chats, while several others process players' inputs and relay state updates back to them. Meanwhile, yet another component might be tasked with the complex calculations of game physics. This phenomenon is widely known as _concurrency_. Importantly, the successful operation of such a concurrent program doesn’t necessarily rely on multiple processing cores, though their presence can certainly enhance performance and responsiveness.

On the other hand, _parallel_ programs are typically centered around solving a single problem. For example, the act of summing the numbers in a large stream can be done sequentially; however, we prefer to split the large stream into smaller segments, and have one core dedicated to summing one segment, essentially allowing many cores to work in _parallel_ to compute the main result. This is known as _parallelism_. Similarly, the functionality of a parallel program doesn’t inherently depend on the availability of multiple cores.

Another key distinction between concurrent and parallel programs is how they engage with the outside world. By their very nature, concurrent programs are in constant interaction with networking protocols, databases, and similar systems. In contrast, a typical parallel program tends to be more focused in its operation. It streams in data, processes it intensively for a period, and then outputs the results, with minimal further I/O during that time.

The lines between concurrency and parallelism can often be blurred, particularly in traditional programming languages that compel developers to utilize the same constructs for both approaches.

In this chapter, we will see how functional programming concepts can be applied to concurrency and parallelism. For our course, assume that all our concurrent and parallel programs operate within the confines of a single OS process. We will then briefly look at some pitfalls of traditional concurrent and parallel programming, and see how purely functional languages tackle these.


[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-26%20OCT%202024-57ffd8?style=for-the-badge
