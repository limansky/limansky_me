---
title: Getting started with shapeless
tags: Scala, shapeless
---

Sooner or later, every Scala programmer trying to study
[shapeless](https://github.com/milessabin/shapeless),
[Scalaz](https://github.com/scalaz/scalaz), [Cats](https://github.com/typelevel/cats) and
other libraries, which are not designed to solve one small problem, but were
created to ~~explode your brain~~ change the way you are writing your code, make
it safer and at the same time more generic.  I've tried to do it several times,
but the main problem is that there are no entry point.  There are lot of things
and all of them are quite difficult.

Finally I decide to solve small problems with shapeless to get some patterns
and scenarios how to use it.  So, what kind of problems it can be?  Shapeless
is really useful when you what to process your data in generic way.  The
first problem I'd like to solve is saving case class into a SQL statement. E.g:

```Scala
case class Sale(name: String, date: LocalDateTime, price: BigDecimal)

SqlSaver[Sale].save(st, 1)(Sale("Banana", LocalDateTime.now, 55))
```

This call shall call prepared statement methods for each field taking in
account the field type.  In this example it should be:

```Scala
st.setString(1, sale.name)
st.setTimestamp(2, Timestamp.valueOf(sale.date))
st.setBigDecimal(3, sale.price.underlying)
```

<!--more-->

## HLists

One of most important things in shapeless are Heterogeneous lists.  This data
structure is something in between of lists and tuples.  It contains elements of
different types, but supports list specific operations like folds and maps.
The type of `HList` contains types of all of it elements:

```Scala
scala> import shapeless._
import shapeless._

scala> val a = 5 :: "foo" :: false :: HNil : Int::String::Boolean::HNil
a: shapeless.::[Int,shapeless.::[String,shapeless.::[Boolean,shapeless.HNil]]] = 5 :: foo :: false :: HNil
```

It's obvious what any data class can be represented as `HList`, and if the instance
of `HList` has required *shape*, it can be converted back to the class.
Shapeless provides a converter back and forth called `Generic[T]`:

```Scala
scala> val gen = Generic[Sale]
gen: shapeless.Generic[Sale]{type Repr =
shapeless.::[String,shapeless.::[java.time.LocalDateTime,shapeless.::[scala.math.BigDecimal,shapeless.HNil]]]} = anon$macro$4$1@34638de0

scala> gen.to(Sale("Banana", LocalDateTime.now(), 33))
res0: gen.Repr = Banana :: 2016-11-20T22:00:21.857 :: 33 :: HNil

scala> gen.from(res0)
res1: Sale = Sale(Banana,2016-11-20T22:00:21.857,33)
```

What happened here?  On the first line the instance of `Generic` for our class
`Sale` was defined.  Each `Generic` instance has a type called `Repr` which
represents a *shape* of `HList` corresponding to it. In our case it's
`String :: LocalDateTime :: BigDecimal :: HNil`.  `Generic[T]` has two methods
`to` and `from` to convert from `T` **to** `Repr` and **from** `Repr` to `T`.

## Type classes

The common practice in shapeless usage is to use *type classes*.  This idea is
came from Haskell, where it's a built-in language feature.  In Scala we are
using traits and implicits to emulate this feature.  The main idea is that we'd
like to split the data and behaviour, but still have polymorphism.  For example
we want to save data on disk, and we need to serialize our classes.  The `save`
function doesn't know how to serialize.  In OOP world we *inherit* from some
kind of `Serializable` class:

```Scala
def save(filename: String, data: Serializable) = {
  val file = new FileOutputStream(name)
  file.write(data.toBytes)
  file.close()
}
```

When we using type classes we have a trait, and implicit implementation for
required data types:

```Scala
trait Serializable[T] {
  def toBytes(t: T)
}

implicit val fooSerializable = new Serializable[Foo] {
  def toBytes(t: Foo) = ???
}

def save[T](filename: String, data: T)(implicit ser: Serializable[T]) = {
  val file = new FileOutputStream(name)
  file.write(ser.toBytes(data))
  file.close()
}
```

Now the data decoupled from serialization.  We can have several class type implementations
for the same data type.  And all of them are implemented separately
from data classes.  Thanks to implicit parameters, we still don't need to pass
any additional parameters to `save` function.  From the caller side, the code
is the same with the OOP inheritance.

## Implementation

Let's define a trait `SqlSaver` which will represent our type class of 
something what can be saved to the statement:

```Scala
trait SqlSaver[A] {
  def save(statement: PreparedStatement, idx: Int)(a: A): Int
}
```

This is quite straightforward thing.  We have type [A] and we can save it into
statement. It takes an index as a parameter, and returns the next available
index.  The other common thing is using of *summoner* or *materializer* method
to be able to create instance in a way like `val saver = SqlSaver[Sale]`:

```Scala
object SqlSaver {
  def apply[T](implicit saver: SqlSaver[T]): SqlSaver[T] = saver
}
```

Now we can create an instances of our type classes for a different data types.
Let's start with a primitive types:

```Scala
object SqlSaver {
  // ...

  def createSaver[A](f: (A, PreparedStatement, Int) => Int): SqlSaver[A] = new SqlSaver[A] {
    override def save(statement: PreparedStatement, idx: Int)(a: A): Int = {
      f(a, statement, idx)
    }
  }

  def createSimpleSaver[A](f: (A, PreparedStatement, Int) => Unit): SqlSaver[A] = 
    createSaver((a, s, i) => {
      f(a, s, i)
      i + 1
    })

  implicit val stringSaver: SqlSaver[String] = createSimpleSaver((a, s, i) => s.setString(i, a))

  implicit val intSaver: SqlSaver[Int] = createSimpleSaver((a, s, i) => s.setInt(i, a))

  implicit val localDTSaver: SqlSaver[LocalDateTime] =
    createSimpleSaver((a, s, i) => s.setTimestamp(i, Timestamp.valueOf(a)))

  implicit val bigDecimalSaver: SqlSaver[BigDecimal] = 
    createSimpleSaver((a, s, i) => s.setBigDecimal(i, a.underlying))
}
```

First of all let's create a helper functions `createSaver` and
`createSimpleSaver` which construct `SqlSaver` instances (in the second one we
assume that only one value is saved, so the next index is always equal the
previous one plus one).  Then we create instances for strings, local dates,
integers and big decimals.  Pretty straightforward, right?  Let's test that it
works (I'm using ScalaTest with Mockito):

```Scala
class SqlSaverTest extends FlatSpec with Matchers with MockitoSugar {

  import SqlSaver._

  val stm = mock[PreparedStatement]

  "SqlSaver" should "save primitive types" in {
    SqlSaver[Int].save(stm, 1)(5) should equal(2)
    verify(stm).setInt(1, 5)

    SqlSaver[String].save(stm, 2)("test me") should equal(3)
    verify(stm).setString(2, "test me")
  }
}
```

All works fine.  But we want to save "custom" classes.  Now it's time to add
Shapeless.  If we will be able to save `HList`s, we can use `Generic`s and
shapeless convert a data to `HList`s for us.

As a regular Scala `List`s, an `HList` has a head and a tail. The last element
is an empty `HList` called `HNil`. Let's start with `HNil`.

```Scala
implicit val hnilSaver: SqlSaver[HNil] = createSaver((_, _, i) = i)
```

This one is simple, because it do nothing. Now, to save non-empty HList we have
to save its head and tail. So we need `SqlSaver`s both for head and tail:

```Scala
implicit def hlistSaver[H, T <: HList](implicit
     hSaver: SqlSaver[H],
     tSaver: SqlSaver[T]
  ): SqlSaver[H :: T] = createSaver {
    case (h :: t, stm, idx) =>
      hSaver.save(stm, idx)(h)
      tSaver.save(stm, idx + 1)(t)
  }
```

Once we can save `HList`s we can create an implicit for `Generic`s:

```Scala
implicit def genericSaver[A, R](implicit
     gen: Generic.Aux[A, R],
     saver: SqlSaver[R]
  ): SqlSaver[A] =
    createSaver((v, stm, idx) => saver.save(stm, idx)(gen.to(v))) 
```

This thing has two type parameters. Type `A` is a data type we'd like to save.
Type `R` is our `HList` type.  The `Generic.Aux` is a shorthand to write
`Generic[A] { type Repr = R }`.  This Aux pattern is really common in shapeless
and well described in this
[post](http://gigiigig.github.io/posts/2015/09/13/aux-pattern.html).
Generally, this pattern idea is to extract dependent type from parameterized
type.  In our case `Generic.Repr` type depends on `Generic` type parameter `A`.
There is only one type `R` corresponding to type `A`.

Let's test it:

```Scala
it should "save case classes" in {
  case class Sale(name: String, date: LocalDateTime, price: BigDecimal)

  val date = LocalDateTime.now()
  SqlSaver[Sale].save(stm, 3)(Sale("foo", date, 5.5)) should equal(6)
  verify(stm).setString(3, "foo")
  verify(stm).setTimestamp(4, Timestamp.valueOf(date))
  verify(stm).setBigDecimal(5, java.math.BigDecimal.valueOf(5.5))
}
```

## Summary

We got expected behaviour with only 50 lines of code.  Even though this code
looks quite ~~weird~~ difficult, it is quite straightforward, and once you
understand it you will read easily.  I think it is simpler than the same
feature implemented with macros.  Also it's really easy to extend:  the only
thing you need to do is to write more implicits for your cases.
