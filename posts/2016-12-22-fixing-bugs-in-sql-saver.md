---
title: Fixing bugs in SqlSaver
tags: Scala, shapeless
---

In the [previous post](/2016-11-24-getting-started-with-shapeless.html)
I created `SqlSaver` class.  Later, playing with it, I found that it has
several bugs.

First of all, it doesn't work properly with nested
classes.  Let's start with a test:


```Scala
case class SaleRecord(id: Int, sale: Sale, seller: String)

it should "save nested case classes" in {
  val date = LocalDateTime.now
  SqlSaver[SaleRecord].save(stm, 6)(
    SaleRecord(1, Sale("bar", date, 42), "Shop")
  ) should equal(11)

  verify(stm).setInt(6, 1)
  verify(stm).setString(7, "bar")
  verify(stm).setTimestamp(8, Timestamp.valueOf(date))
  verify(stm).setBigDecimal(9, java.math.BigDecimal.valueOf(42))
  verify(stm).setString(10, "Shop")
}
```

Unfortunately it doesn't compile:

```
[error] SqlSaverTest.scala:38: diverging implicit expansion for type SqlSaver[LocalDateTime :: BigDecimal :: HNil]
[error] starting with method hlistSaver in object SqlSaver
[error]    SqlSaver[SaleRecord].save(stm, 6)(
[error]            ^
```

    Note: here and later I rewrote HLists into the infix form, for readability.

<!--more-->

That's strange.  We know that SqlSaver for `Sale` can be instantiated, because
`Sale` contains only fields of supported types.  Maybe shapeless cannot
construct `Generic` for our nested classes? If we try to do it in REPL we get
the following result:

```Scala
Generic[SaleRecord]{type Repr = Int :: Sale :: String :: HNil }
```

But if we try to evaluate `SqlSaver[Int :: Sale :: String :: HNil]` we get an
error.  The problem we are faced with is related to how Scala implicit resolution
works.  This topic is described in "The Type Astronaut's Guide to Shapeless".  The
main idea is that the Scala compiler tries to avoid infinite loops during implicit
resolution.  To do that, it has several heuristics. One of them is to stop
searching if it meets the same step twice.  Another one is to stop if the
complexity of type parameters is increasing for the type constructor it met
before.  In shapeless one of the type constructors is `::[H, T]` -- the
constructor of HList. In our case we get a more complex HList for Sale than for
SaleRecord, so it cannot find an implicit instance of `SqlSaver[Sale]` and doesn't compile.
Fortunately shapeless has special type `Lazy` to solve this problem (else shapeless
would be a quite useless thing).  Let's fix the last error case:

```Scala
implicit def hlistSaver[H, T <: HList](implicit
     hSaver: Lazy[SqlSaver[H]],
     tSaver: SqlSaver[T]
  ): SqlSaver[H :: T] = createSaver {
    case (h :: t, stm, idx) =>
      hSaver.value.save(stm, idx)(h)
      tSaver.save(stm, idx + 1)(t)
  }
```

Once we wrapped the `hSaver` in `Lazy` it prevents the compiler from being too
clever, and postpones the implicit parameters evaluation to runtime.  Now the
`SqlSaver` for HList works properly.  We can fix the `genericSaver` in the
same way, wrapping `saver` into `Lazy`:

```Scala
implicit def genericSaver[A, R](implicit
     gen: Generic.Aux[A, R],
     saver: Lazy[SqlSaver[R]]
  ): SqlSaver[A] =
    createSaver((v, stm, idx) => saver.value.save(stm, idx)(gen.to(v)))
```

Now the test compiles successfully but fails on runtime with "9 did not equal
11" message. What happened? The current implementation of HList saver assumes that the head
saver takes only one element.  This worked for primitive types, but of course
doesn't work for classes.  To fix that we need to use the next index returned by
`hSaver`:

```Scala
implicit def hlistSaver[H, T <: HList](implicit
     hSaver: Lazy[SqlSaver[H]],
     tSaver: SqlSaver[T]
  ): SqlSaver[H :: T] = createSaver {
    case (h :: t, stm, idx) =>
      val next = hSaver.value.save(stm, idx)(h)
      tSaver.save(stm, next)(t)
  }
```

Now it works fine for nested classes.
