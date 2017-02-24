---
title: How does shapeless works?
tags: Scala, shapeless
previewButton: Let's code
background: ship.jpg
---

In the last three posts I used shapeless to make several pieces of code.  It
works, it is easier than writing macros manually, but it's a bit magical.
Well...  To be honest, it's completely magic.  I think what the best way to
study something is try to make a simple copy to get the main principles, but
avoid unnecessary details.

<!--more-->

`HList` is one of a most important shapeless' building blocks, so, let's start
with `HList` implementation. 

```Scala
sealed trait HList
```

It's just a sealed trait with two implementations.  The first one is `::` which
have a head and tail:

```Scala
case class ::[+H, +T <: HList](head: H, tail: T) extends HList {
  def ::[H1](h1: H1): H1 :: H :: T = me.limansky.::(h1, this)

  override def toString: String = s"$head :: $tail"
}
```

We have to define method `::` to be able to chain more than two elements into
`HList`

The second implementation is `HNil` representing empty list:

```Scala
case object HNil extends HList {
  def ::[H](h: H) = me.limansky.::(h, this)
}
```


