---
title: SQL generation with shapeless
tags: Scala, shapeless
---

In the [previous](/posts/2016-11-24-getting-started-with-shapeless.html)
[posts](http://limansky.me/posts/2016-12-22-fixing-bugs-in-sql-saver.html) we
created the `SqlSaver` class which can set values into the prepared
statement.  It assume that the SQL request is correct and the parameters are in
required order (in the order they are defined in the model). What if the model class
will be changed?  If the query is not updated we'll get runtime error, since the
fields order in query and the order of calls performed by `SqlSaver` are not the same
anymore.  So, it would be nice to generate SQL queries as well.  Something like:

```Scala
val query = StatementGenerator[Sale].insert(tableName)
val statement = connection.prepareStatement(query)
SqlSaver[Sale](statement, 1)(sale)
statement.execute()
```

Let's try to implement it with shapeless.

<!--more-->

First let's define our type class for statement generators:

```Scala
trait StatementGenerator[A] {
  def select(table: String): String
  def insert(table: String): String
}
```

Then we can create a companion object with summoner function and
implementation for case classes.  Since we need not only values, but the field names,
we need to use `LabelledGeneric` class instead of `Generic`.

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
instance of type `A` to `HList` `R`.  But we don't have an `A` instance, because we
don't need values.  All we need is the field names.  Shapeless contains package
`ops` which provides utilities for different cases.  We need to get keys of the
key-value records.  Such class called `Keys` is available in package
`ops.record`.  It takes an `HList` of records and provides `HList` of keys.  Next
thing we need to do is to materialize `HList` of keys into Scala List of Symbols
(because key in our case is Symbol).  The utility class we need is called `hlist.ToList`.
We also have to set the constraints for types we use:  type passed to Keys
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

This code is quite straightforward and it works for most cases.  But it doesn't cover all
the features of `SqlSaver`. In the last post we added ability to save nested case
classes.  So, we need to recursively handle all of the fields and make a flat
list of primitive ones.  E.g. if we have classes `A(a: String, b: Int)` and
`B(c: String, d: A, e: Int)` we should get `c :: a :: b :: e :: Nil`.

I think it's better to create separate type class `FieldLister`, which will
provide a list of fields.  Lets start with our type class:

```Scala
trait FieldLister[A] {
  val list: List[String]
}
```

We want to create an instances of `FieldLister` for any class, so we need
`LabelledGeneric` to convert class to `HList`:

```Scala
object FieldLister {
  def genericLister[A, R](implicit
    gen: LabelledGeneric.Aux[A, R],
    lister: Lazy[FieldLister[R]]
  ): FieldLister[A] = new FieldLister[A] {
    override val list = lister.value.list
  }
```

You might notice, that we don't need the value of `LabelledGeneric`.  But we
need it on the type level, to link types `A` and `R`, because type `R` *depends* on
type `A`.

Next, we can create instances for `HList`.  It's obvious that the result list
for `HNil` is `Nil`:

```Scala
implicit val hnilLister: FieldLister[HNil] = new FieldLister[HNil] {
  override val list = Nil
}
```

The instance for non-empty list is more tricky.  We need to separate primitive
types from nested classes.  For the nested classes the implementation is quite
straightforward.  We obtain instances for tail and head and concatenating them:

```Scala
implicit def hconsLister[K, H, T <: HList](implicit
  hLister: Lazy[FieldLister[H]],
  tLister: FieldLister[T]
): FieldLister[FieldType[K, H] :: T] = new FieldLister[FieldType[K, H] :: T] = {
  override val list = hLister.value.list ++ tLister.list
}
```

Lets take a closer look on this function.  To understand what's going on we
need to understand what does the `LabelledGeneric` produce.  We can check it in
REPL:

```Scala
scala> import shapeless._
import shapeless._

scala> case class Test(first: String, second: Int)
defined class Test

scala> LabelledGeneric[Test]
res0: LabelledGeneric[Test]{
  type Repr = String with KeyTag[Symbol with Tagged[String("first")],String] ::
              Int with KeyTag[Symbol with Tagged[String("second")],Int] ::
              HNil
} =
LabelledGeneric$$anon$1@1b09215f
```

I rewrote result type in infix form and removed package names for readability.
What is important here is that the `Repr` type is not just `String :: Int :: HNil`,
but each type element contains additional type level information.

If we check the shapeless sources, we find that `FieldType` is just an type alias:

```Scala
  type FieldType[K, +V] = V with KeyTag[K, V]
```

That's exactly what we saw in `Repr`.  With this knowledge we can rewrite `Repr` type as:

```Scala
LabelledGeneric[Test]{
  type Repr = FieldType[Symbol with Tagged[String("first")],String] ::
              FieldType[Symbol with Tagged[String("second")],Int] ::
              HNil
}
```

And this is the reason why we need to define result type of `hconsLister` as
`FieldLister[FieldType[K, H] :: T]`.  On the value level we just need to concatenate lists
produced for the head and for the tail of the `HList`.

At this point our code can work with case classes and `HLists` of elements for which we have instances of
`FieldLister`, i.e. of another HLists and case classes.  But what with the
primitive types?  If we have a head element of `HList` which does not have an
instance of `FieldLister` we need to get this field name and set it as a head
element of the result list.  We need to get the instance of type `K` on value
level somehow to get the field name.  Shapeless provides type class `Witness`
for this porpose.  With all of these blocks we can build our function:

```Scala
implicit def primitiveFieldLister[K <: Symbol, H, T <: HList](implicit
  witness: Witness.Aux[K],
  tLister: FieldLister[T]
): FieldLister[FieldType[K, H] :: T] = new FieldLister[FieldType[K, H] ::T] {
  override val list = witness.value.name :: tLister.list
}
```

Even though it looks nice, we got a new kind of problem here.  Our implicits are
ambiguous.  Both `hconsLister` and `primitiveFieldLister` can be applied to
`HList`.  Scala compiler cannot choose which one is more applicable (even
thought one of this declarations requires an instance of `FieldLister[H]`, both
of this instances have the same weight).  So, the compiler
requires from you to avoid conflicts in implicit resolution.  To manage
implicit resolution order we can use a "Low priority" pattern.  The idea is to
move the implicits with lower precedence to the parent class.  Once the
compiler can find an implicit instance in the child class it will use it (it
will not search all possible implicits in the class parents).  But if it is not
able to find implicit in the inherited class, it will search in the parent
classes.

Now, when we have the `FieldLister` we can easy implement `StatementGenerator`.
All we need to do is to wrap `FieldLister` result into statements:

```Scala
object StatementGenerator {
  implicit def genericGenerator[A](implicit
    fieldLister: FieldLister[A]
  ): StatementGenerator[A] = new StatementGenerator[A] {
    override def select(table: String): String = {
      val fields = fieldLister.list.mkString(",")
      s"SELECT $fields FROM $table"
    }

    override def insert(table: String) = {
      val fieldNames = fieldLister.list
      val fields = fieldNames.mkString(",")
      val placeholders = List.fill(fieldNames.size)("?").mkString(",")
      s"INSERT INTO $table ($fields) VALUES ($placeholders)"
    }
  }
}

And that's all folks.
