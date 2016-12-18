---
title: Generation SQL requests with shapeless
tags: Scala, shapeless
---

In the previous posts we created a SqlSaver class with can set values in the prepared
statement.  It assume that the SQL request is correct.  What if the model class
will be changed?  If the request is not updated we get runtime error, since the
fields order in not match anymore.  It would be nice to generate requests as
well.  Something like:

```Scala
val query = StatementGenerator[Sale].insert(tableName)
val statement = connection.prepareStatement(query)
SqlSaver[Sale](statement, 1)(sale)
statement.execute()
```

Let's try to implement it with shapeless.

<!--more-->

Since we need not only values, but a field names, we need to use
`LabelledGeneric` class instead of `Generic`.

First let's define our type class for statement generators:

```Scala
trait StatementGenerator[A] {
  def select(table: String): String
  def insert(table: String): String
}
```

Then we can create a companion object with summoner function and
implementation:

```Scala
object StatementGenerator {
  def apply[A](implicit sg: StatementGenerator[A]): StatementGenerator[A] = sg

  def genericGenerator[A, R](implicit
    gen: LabelledGeneric.Aux[A, R]
  ): StatementGenerator[A] = new StatementGenerator[A] {
    override def select(table: String): String = ???
    override def insert(table: String): String = ???
  }
}
```

Ok, we have an instance of `LabelledGeneric` for type `A` which can convert an
instance of type `A` to HList `R`.  But we don't have an instance.  And we
don't need values.  All we need is field names.  Shapeless contains package
`ops` which provides utilities for different cases.  We need to get keys of the
key-value records.  Such class called `Keys` is available in package
`ops.record`.  It takes an HList of records and provides HList of keys.  Next
thing we need to do is materialize HList of keys into Scala List of Symbols
(because key in our case is Symbol).  The class we need called `hlist.ToList`.
We also need to add some constraints on types we use:  type passed to Keys
shall be an `HList`, as well as a type passed to `ToList`.  Let's code:

```Scala
object StatementGenerator {
  def apply[A](implicit sg: StatementGenerator[A]): StatementGenerator[A] = sg

  def genericGenerator[A, R <: HList, K <: HList](implicit
    gen: LabelledGeneric.Aux[A, R],
    keys: record.Keys.Aux[R, K],
    ktl: hlist.ToList[K, Symbol]
  ): StatementGenerator[A] = new StatementGenerator[A] {
    override def select(table: String): String = ???
    override def insert(table: String): String = ???
  }
}
```

Once we have all required components we can implement select and insert
methods:

```Scala
override def select(table: String): String = {
  val fields = keys().toList.map(_.name).mkString(",")
  s"SELECT $fields FROM $table"
}

override def insert(table: String): String = {
  val fieldNames = keys().toList.map(_.name)
  val fields = fieldNames.mkString(",")
  val placeholders = List.fill(fieldNames.size)("?").mkString(",")
  s"INSERT INTO $table ($fields) VALUES($placeholders)"
}
```
