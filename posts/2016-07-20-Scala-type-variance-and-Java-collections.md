---
title: Scala type variance and Java collections
tags: Scala, collections, Java
---

Converting Scala collections to Java and backward
-------------------------------------------------

It's quite often that we need to use a Java API from Scala.  And these usages occurs even more
often when you have modules written in Java in your Scala project.  The most
common problem is to pass Java collections to Scala API and Scala collections
to Java.  Fortunately, Scala library provides bidirectional implicit converters
between Scala and Java collections.  There are two classes `JavaConverters` and
`JavaConversions` which provide the same functionality but in a different way.

Assume we have a Java service:

```Java
interface Message {
    String getText();
    LocalDateTime getDate();
}

public class JavaService {
    void handleMessages(List<Message> messages) {
        messages.stream()
            .sorted((o1, o2) -> o1.getDate().compareTo(o2.getDate()))
            .forEach(m ->
                System.out.println(
                    m.getDate().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
                    + " " + m.getText()));
    }

    List<Message> generateMessages(int n) {
        return IntStream.range(0, n)
                .mapToObj(i -> new JavaMessage(String.valueOf(i), LocalDateTime.now()))
                .collect(Collectors.toList());
    }
}

```
<!--more-->

And a Scala code to work with messages (the same code in Scala):

```Scala
class ScalaService {

  def handleMessages(messages: Seq[Message]): Unit = {
    messages.sortWith((a, b) => a.getDate.isBefore(b.getDate)).foreach(m =>
      println(s"${m.getDate.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)} ${m.getText}")
    )
  }

  def generateMessages(n: Int): Seq[Message] = {
    0 until n map (i => MessageImpl(i.toString, LocalDateTime.now()))
  }
}
```

And we have two different Message implementations in Java and in Scala, which
are really simple (just a POJO for Java and a case class for Scala).

Now let's try to use `JavaConversions`, which gives you an implicit magic:

```Scala
import scala.collection.JavaConversions._

javaService.handleMessages(scalaService.generateMessages(3))
scalaService.handleMessages(javaService.generateMessages(3))
```

In contrast with `JavaConversions` `JavaConverters` require us to explicitly specify what
needs to be converted explicitly, using `asScala`, `asJava` or one of the other
methods:

```Scala
import scala.collection.JavaConverters._

javaService.handleMessages(scalaService.generateMessages(3).asJava)
scalaService.handleMessages(javaService.generateMessages(3).asScala)
```

What is the reason to have two classes providing similar functionality?  I
think the `JavaConversions` gives you more simplicity.  But the
`JavaConverters` gives you more control.  You can handle what's going on, what
type do you pass to a Java code.

The problem
-----------

Now, let's try this:

```Scala
val messages = List("foo", "bar", "baz").map(s => ScalaMessage(s, LocalDateTime.now()))

scalaService.handleMessages(messages)
javaService.handleMessages(messages)
```

This works fine with `JavaConversions`, but it doesn't compile with
`JavaConveters`:

```
type mistmatch

found: java.util.List[ScalaMessage]
required: java.util.List[Message]
```

What happened here? Why did we get a list of `ScalaMessage`s here?  Because Scala
always tries to infer the most precise type.  In this case it is
`List[ScalaMessage]` because all list elements are `ScalaMessage`s.  Then, why
did `ScalaService` take it as a parameter without any error?  Even though these
methods signature looks same, there is a big difference in Scala and Java
versions.  Scala list is a covariant type.  This means that the list of subtypes
of `Message` is a subtype of list of `Message`.  As result, we can use a
`List[ScalaMessage]` as a `List[Message]`.

But there are no type variance in Java.  We can emulate it, defining
handleMessages as:

```Java
void handleMessages(List<? extends Message> messages)
```

But what if it's a library method, or we don't want to change the method
signature for some other reason?  And how does `JavaConversions` manage to solve it?

Let's check how `JavaConversions` works.  When the Scala compiler find that an
argument type is not compatible with a required type, it tries to find an
implicit conversion which can change that type to the required one.  In our case the
required type is a `java.util.List[Message]`, but the actual type is
`scala.collections.List[ScalaMessage]`.  So, the compiler tries to find an
implicit way to convert the actual type to the expected one.  In this case it uses
`seqAsJavaList` function from `JavaConversions` which have the following definition:

```Scala
implicit def seqAsJavaList[A](seq: Seq[A]): java.util.List[A]
```

This function has a required result type (`java.util.List[Message]`) and takes
a `Seq[Message]` as an argument.  But the Scala `List` is derived from `Seq`,
so we can pass the List to the places where Seq is required.  `Seq` type
parameter is covariant and because of that `List[ScalaMessage]` inherits `Seq[Message]`,
and it can be passed to the `seqAsJavaList[Message]`.

Now let's check the `JavaConverters` method, to understand why it doesn't work.
This class uses another type of Scala implicit conversions.  When the Scala compiler
finds a method call of unknown method it tries to find an implicit conversion
which will return a type with this method.
In our case following implicit conversion is used:

```Scala
implicit def seqAsJavaListConverter[A](b : Seq[A]): AsJava[java.util.List[A]]
```

This method takes a sequence of type `A` and returns an instance of proxy class
`AsJava` (as you might guess, it has `asJava` method) of Java list of same type
`A`. Here, we have `AsJava[java.util.List[ScalaMessage]]`.  So, the
result of the `asJava` call is a Java list of `ScalaMessage`,  which cannot be
passed as a list of `Message` in Java.

The solution
------------

Well, what can we do?

1. We can just specify type of the `messages` variable:

```Scala
val messages: List[Message] = List("foo", "bar", "baz")
  .map(s => ScalaMessage(s, LocalDateTime.now()))

javaService.handleMessages(messages.asJava)
```

It works, but what if we'd like to inline it?

```Scala
javaService.handleMessages(List("foo", "bar", "baz")
  .map(s => ScalaMessage(s, LocalDateTime.now())).asJava)
```

And we get the same error as previously.

2. We can cast the result type:

```Scala
javaService.handleMessages(List("foo", "bar", "baz")
  .map(s => ScalaMessage(s, LocalDateTime.now())).asInstanceOf[List[Message]].asJava)
```

This also works, but it looks ugly.

3. We can try to write our own converter. We will use an implicit class to wrap a
   `Seq[A]`.  This class will have a method `asJava[B]` which returns Java list
   of `B`.  But we cannot just put instances of `A` in the collection of
   arbitrary `B`.  Fortunately we don't need to do that.  We are interested only in `B`s 
   which are parents of `A`, because such cast is always safe. So the code is:

```Scala
object Converters {
  implicit class AsJava[A](val a: Seq[A]) extends AnyVal {
    def asJava[B >: A]: java.util.List[B] = {
      import scala.collection.convert.WrapAsJava._
      seqAsJavaList(a)
    }
  }
}
```

And now our problem code is compiled:

```Scala
import Conveters._

javaService.handleMessages(List("foo", "bar", "baz")
  .map(s => ScalaMessage(s, LocalDateTime.now())).asJava)
```

We can specify the type `B` explicitly (like `messages.asJava[Message]`), but the
Scala implicit resolution is smart enough to infer the required type itself.
