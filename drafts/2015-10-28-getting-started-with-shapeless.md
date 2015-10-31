---
title: Getting started with shapeless
tags: Scala, shapeless
---

Sooner or later, every Scala programmer trying to study
[shapeless](https://github.com/milessabin/shapeless),
[Scalaz](https://github.com/scalaz/scalaz) or
other libraries, which are not designed to solve one small problem, but was
created to ~~explode your brain~~ change the way you are writing your code, make
it safer and at the same time more generic.  I tried to do it several times,
but the main problem is that there are no entry point.  There are lot of things
and all of them are quite difficult.

Finally I decide to solve small problems with shapeless to get some patterns
and scenarios how to use it.

The first problem I'd like to solve is serialization into `Map`.
For example we have class `Person` and we want to convert it into `Map`:

```Scala
case class Person(name: String, age: Int, knowsScala: Boolean)

// Map(name -> John, age -> 42, knowsScala -> false)
println("Mapped person is", Mapper.toMap(Person("John", 42, false)))
```

<!--more-->

This problem can be solved using macros, as described in [this
post](http://blog.echo.sh/2013/11/04/exploring-scala-macros-map-to-case-class-conversion.html).
But there is lot of boilerplate code.  Shapeless allows us to avoid writing of
the similar macros code each time we want to inspect some class (of course it
allows us much more, but we don't need it for this task).

HLists
------

One of most important things in shapeless are Heterogenous lists.  This data
structure is something in between of lists and tuples.  It contains elements of
different types, but supports list specific operations like folds and maps.
The type of HList contains types of all of it elemets:

```Scala
scala> import shapeless._
import shapeless._

scala> val a = 5 :: "foo" :: false :: HNil : Int::String::Boolean::HNil
a: shapeless.::[Int,shapeless.::[String,shapeless.::[Boolean,shapeless.HNil]]] = 5 :: foo :: false :: HNil
```

Scala writes type of value `a` using method calls, but we can specify it
explicitely like `Int::String::Boolean::HNil`.  Now it possible to call map. But
how? HList contains object of different types.  To solve this problem, we need
to pass **polimorphic function** to map. Let's define function `toStr`:

```Scala
object toStr extends Poly1 {
  implicit def caseInt = at[Int](x => String.valueOf(x))
  implicit def caseStr = at[String](x => x)
  implicit def caseBool = at[Boolean](x => if (x) "Yeah!" else "Sucks")
}
```

This object is a polymorphic function with one parameter, which behavior is
depends on argument type.  Now, it possible to map the list:

```Scala
scala> val b = a map toStr
b: shapeless.::[String,shapeless.::[String,shapeless.::[String,shapeless.HNil]]] = 5 :: foo :: Sucks :: HNil

scala> b.toList
res7: List[String] = List(5, foo, Sucks)
```

So, we converted HList of Int, String and Boolean to HList of String, and then
converted it to Scala List.  What will happen if polymorphic function doesn't
have clause to handle data type?  The code will not compile:

```Scala
scala> val c = 5.0 :: a c: shapeless.::[Double,shapeless.::[Int,shapeless.::[String,shapeless.::[Boolean,shapeless.HNil]]]] = 5.0 :: 5 :: foo :: false :: HNil

scala> c map toStr
<console>:17: error: could not find implicit value for parameter mapper: shapeless.ops.hlist.Mapper[toStr.type,shapeless.::[Double,shapeless.::[Int,shapeless.::[String,shapeless.::[Boolean,shapeless.HNil]]]]]
```

Ok, but how does it related to serialization problem?  Since HList is strong
typed we can convert a class into HList, do some transformations and convert it
back or to another class.  For example class `case class Foo(a: String, b: Int)`
can be represented as `String::Int::HNil`.  And you don't need to write this
converters yourself.  Shapeless provides class `Generic[T]` to perform these
transformations.

```Scala
scala> case class Foo(a: String, b: Int)
defined class Foo

scala> val fooGen = Generic[Foo]
fooGen: shapeless.Generic[Foo]{type Repr = shapeless.::[String,shapeless.::[Int,shapeless.HNil]]} = fresh$macro$3$1@5ee7c65c

scala> fooGen.to(Foo("Some magic", 42))
res1: fooGen.Repr = Some magic :: 42 :: HNil

scala> fooGen.from("Hi there":: 33 :: HNil)
res2: Foo = Foo(Hi there,33)
```

What has happened here?  `fooGen` is an instance of Generic for `Foo`.  And it
have type alias `Repr` for the `HList` of required *shape*.  Now, assume we
have `case class Bar(z: String, y: Int)` and `Generic` instance for it.  Since
`Foo` and `Bar` have same shape, we can convert `Bar` into `Foo` using
generics:

```Scala
scala> fooGen.from(barGen.to(Bar("Some data for Foo", 69)))
res3: Foo = Foo(Some data for Foo,69)
```
