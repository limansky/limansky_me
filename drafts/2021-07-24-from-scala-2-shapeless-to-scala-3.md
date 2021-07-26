---
title: From Scala 2 shapeless to Scala 3
tags: Scala, Scala 3, shapeless
---

In this post I'd like to discover Scala 3 generic programming abilities.  Scala 3 provides a lot of new features.  And
generic programming is one of the areas where we have a lot of changes.  I assume, that you have used 
[shapeless](https://github.com/milessabin/shapeless) with Scala 2, but if not, I'll try to explain things in this post.
However I'd recommend to read [this post][1] before if you don't even know that shapeless is.

<!--MORE-->

### Tuples in Scala 3

Let's take a look at tuples in Scala 3.  In previous versions of Scala we had famous `Tuple1` .. `Tuple22` classes
defined like:

```Scala
final case class Tuple2[+T1, +T2](_1: T1, _2: T2) 
    extends Product2[T1, T2] 
    with Product 
    with Serializable
```

These classes are available in Scala 3 too, but there is another classes for tuples:

```Scala
sealed trait Tuple extends Product
object EmptyTuple extends Tuple
trait NonEmptyTuple extends Tuple
case class [+H, +T <: Tuple] *:(head: H, tail: T) extends NonEmptyTuple
```

I simplified the code a little bit, but the idea is the same.  This is something new in the Scala library, but it looks
very familiar.  Actually it's a list with different types of its' elements.  This called _Heterogeneous list_, or `HList`!
In Scala 2 we had `HList`s in shapeless,  and also in some other libraries.  Having heterogeneous lists in standard library
makes a lot of sense, and now we don't need any other HList implementation.  Scala 3 tuples has a lot of useful functions, for
example they can be converted to `List`, be concatenated, zipped, etc.:

```Scala
scala> val t = (5, "String", 3d, false)
val t: (Int, String, Double, Boolean) = (5,String,3.0,false)

scala> t.toList
val res0: List[Tuple.Union[t.type]] = List(5, String, 3.0, false)

scala> val x = t.drop(2)
val x: (Double, Boolean) = (3.0,false)

scala> t ++ x
val res1: Int *: String *: Double *:
  scala.Tuple.Concat[Boolean *: scala.Tuple$package.EmptyTuple.type, x.type]
    = (5,String,3.0,false,3.0,false)
```

Moreover Scala 3 provides mechanisms similar to shapeless `Generic`s making possible to convert from algebraic data types
(if you not familiar with algebraic data types, to understand further examples you might think that it's just case classes)
to tuples and back.  Let's take a look on these features.

### Tuples from case classes

The `Tuple` companion object contains method `fromProductTyped` which allows us to construct tuple from a case class:

```
scala> case class Foo(a: String, b: Int)
// defined case class Foo

scala> Tuple.fromProductTyped(Foo("test", 5))
val res2: (String, Int) = (test,5)
```

With this knowledge we can try to implement `SqlSaver` from ["Getting started with shapeless"][1] post for Scala 3.  So, let's do it.
The type class definition itself is keep unchanged:

```Scala
trait SqlSaver[A] {
  def save(statement: PreparedStatement, idx: Int)(a: A): Int
}
```

However, there is a new syntax for implicits Scala 3.  When we need to use an implicit instance we use `using` keyword.  So,
now the summoner method will look like:

```Scala
object SqlSaver {
  def apply[T](using ss: SqlSaver[T]): SqlSaver[T] = ss
}
```

When we need to declare an instance of type class (or any other implicit), we use `given` keyword.  With this new syntax instances of `SqlSaver` for
primitive types become:

```Scala
object SqlSaver {
  // ...
  given SqlSaver[Int] = createSimpleSaver((a, s, i) => s.setInt(i, a))
  given SqlSaver[String] = createSimpleSaver((a, s, i) => s.setString(i, a))
  given SqlSaver[Double] = createSimpleSaver((a, s, i) => s.setDouble(i, a))
  given SqlSaver[BigDecimal] = createSimpleSaver((a, s, i) => s.setBigDecimal(i, a.underlying))
  given SqlSaver[LocalDateTime] = 
    createSimpleSaver((a, s, i) => s.setTimestamp(i, Timestamp.valueOf(a)))
```

In this example we created anonymous instances, but it's also possible to give names to the type class instances, e.g.:

```Scala
  given intSaver: SqlSaver[Int] = createSimpleSaver((a, s, i) => s.setString(i, a))
```

Now, let's implement `SqlSaver` instances for tuples.  As it was before for `HNil` (empty `HList`), for the empty tuple it just do nothing:

```Scala
  given SqlSaver[EmptyTuple] = createSaver((_, _, i) => i)
```

For non-empty tuple we need `SqlSaver` for head, to save left tuple member and `SqlSaver` for the tail, like we did before
for `::` (non-empty `HList`):

```Scala
  given [H, T <: Tuple](using hSaver: SqlSaver[H], tSaver: SqlSaver[T]): SqlSaver[H *: T] = 
    new SqlSaver[H *: T] {
      override def save(statement: PreparedStatement, idx: Int)(t: H *: T): Int = {
        val next = hSaver.save(statement, idx)(t.head)
        tSaver.save(statement, next)(t.tail)
      }
    }
```

Here, we created instance for tuple dependent on the instances for the `H` and `T` via `using` keyword.

Finally, we can create instance for `Product`s, which will convert a case class to tuple, and then call `SqlSaver`
for the tuple to really save the data.  However to do it we need to know exact tuple type.  For example if the product is
`case class Foo(a: String, b: Int)` then the tuple type will be `(String, Int)`, or, that's the same `String *: Int *: EmptyTuple`.
In shapeless for Scala 2 we used `Generic` to convert ADTs to and from `HList`s.  It also was a type link between ADTs and their
HList representations.  In Scala 3 we have class `Mirror` to connect products and coproducts with tuples both on type and value level.

To achieve that `Mirror` trait contains several type members.  The `MirroredElemTypes` is a tuple type we are looking for.  Bearing this
in mind, we can connect the mirror with the `SqlSaver` in the `using` part of the type class instance declaration:

```Scala
import scala.deriving.Mirror
// ...

  given [P <: Product](using m: Mirror.ProductOf[P],
                            ts: SqlSaver[m.MirroredElemTypes]
                      ): SqlSaver[P] = new SqlSaver[P] {
    override def save(statement: PreparedStatement, idx: Int)(t: P): Int =
      ts.save(statement, idx)(Tuple.fromProductTyped(t))
  }

```

Here you can see another one cool feature of Scala 3.  Previously we had to use Aux pattern to make types depend on each other.
Now, we can just use type members in another function parameters or even as a result type.

### Summary

Scala 3 brings us a lot of new features.  Personally I like the way the language is evolving.  Even if some things a bit
controversial most of the stuff makes Scala more readable, and gives us tools to build standard solutions for standard problems.
Typelevel programming in general is complex topic, but the new code looks a bit simpler, and also it doesn't require external
dependencies.

[1]: /posts/2016-11-24-getting-started-with-shapeless.html
