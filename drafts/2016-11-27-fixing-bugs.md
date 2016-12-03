---
title: Fixing bugs in SqlSaver
tags: Scala, shapeless
---

In the [previous post](/2016-11-24-getting-started-with-shapeless.html) class
`SqlSaver` was created.  I've found that it doesn't work properly with nested
classes.  Let's start with the test:


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
[error] SqlSaverTest.scala:38: diverging implicit expansion for type SqlSaver[shapeless.::[java.time.LocalDateTime,shapeless.::[BigDecimal,shapeless.HNil]]]
[error] starting with method hlistSaver in object SqlSaver
[error]    SqlSaver[SaleRecord].save(stm, 6)(
[error]            ^
```

<!--more-->

That's strange.  We know that SqlSaver for `Foo` can be instantiated, because
all `Foo` contains only fields of supported types.  Maybe sheless cannot
construct generic for our nested classes?  Let's try in REPL:
