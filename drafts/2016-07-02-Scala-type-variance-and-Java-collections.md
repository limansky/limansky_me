---
title: Scala type variance and Java collections
tags: Scala, collections
---

It's quite often when we need to use a Java API from Scala.  And it's really
often when you have modules written in Java in your Scala project.  The most
common problem is to pass Java collections to Scala API and Scala collections
to Java.  Fortunately, Scala library provides bidirectional implicit converters
between Scala and Java collections.  There are two classes `JavaConverters` and
`JavaConversions` which provide a same functionality but in the different way.

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
are really simple (just a PoJo for Java and case class for Scala).

Now let's try to `JavaConversions` gives you an implicit magic:

```Scala
import scala.collection.JavaConversions._

javaService.handleMessages(scalaService.generateMessages(3))
scalaService.handleMessages(javaService.generateMessages(3))
```

In contrast with `JavaConversions` `JavaConverters` require to specify what is
need to be converted explicitly, using `asScala`, `asJava` or one of the other
methods:

```Scala
import scala.collection.JavaConverters._

javaService.handleMessages(scalaService.generateMessages(3).asJava)
scalaService.handleMessages(javaService.generateMessages(3).asScala)
```

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

What happen here? Why do we get a list of `ScalaMessage`s here?  Because Scala
always tries to infer most precise type.  In this case it is
`List[ScalaMessage]` because all list elements are `ScalaMessage`s.  When, why
`ScalaService` takes it as a parameter without any error?  Even though these
methods signature looks same, there is a big difference in Scala and Java
versions.  Scala list is covariant type.  This means that the list of subtypes
of `Message` is a subtype of list of `Message`.  As result, we can use a
`List[ScalaMessage]` as a `List[Message]`.

But there are no type variance in Java.  We can emulate it, defining
handleMessages as:

```Java
void handleMessages(List<? extends Message> messages)
```

But what if it's a library method, or we don't want to change the method
signature for some other reason?  And how `JavaConversions` manages to solve it?

The difference is in the type resolution.
