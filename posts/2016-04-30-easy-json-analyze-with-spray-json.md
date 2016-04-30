---
title: Easy JSON analyze with spray-json
tags: Scala, Akka, Spray, DSL
---

If you use [Spray](http://spray.io) or [Akka HTTP](http://akka.io) to
create REST service, possible you need to work with JSON objects.  Both Spray
and Akka HTTP have built-in support of [spray-json](https://github.com/spray/spray-json).

In most cases you have fixed set of fields in your JSON API, so the proposed
way to work with spray-json is to create model case classes and
marshallers/unmarshallers for them:

```Scala
case class Person(name: String, age: Int, title: Option[String])

trait PersonRoutes extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val personFormat = jsonFormat3(Person)

  val route =
    put {
      path("people") {
        entity(as[Person]) { person =>
          // Store to db
          compete(StatusCodes.Created)
        }
      }
    }
}
```
<!--more-->

In this example implicit formatter do all JSON validation and deserialization
work.  So `person` is an instance of class `Person`.  But sometimes you might
not have strict model, but just some JSON object.  You can represent this data
as instance of `JsObject` class which is defined as:

```Scala
case class JsObject(fields: Map[String, JsValue]) extends JsValue
```

So, if we'd like to add some schemeless information to our `Person` class we
can just add JsObject field.

```Scala
case class Person(name: String, age: Int, title: Option[String], extras: JsObject)
```

The problem is that this is not really useful to inspect `JsObject`.  The only
thing you can do with it is to get its fields like a `Map[String, JsValue]`.

For example some of the `Person` objects have address information:

```JavaScript
{
    "name" : "John Doe",
    "age" : 42,
    "extras" : {
        "address" : {
            "city"  : "Moscow",
            "street" : "Zemlyanoy Val"
        }
    }
}
```

Assume we'd like to inspect these objects and pass only if the city is Moscow.
To do that we can write something like:

```Scala
entity(as[Person]) { person =>
  val city = for {
    addr <- person.extras.fields.get("address")
    ao   <- Try(addr.asJsObject).toOption
    c    <- ao.fields.get("city")
  } yield c

  if (city.map(_ == "Moscow").getOrElse(false)) {
    complete(StatusCodes.Ok)
  } else {
    complete(StatusCodes.BadRequest)
  }
}
```

This code looks quite ugly.  Even with two layers we have a lot of
boilerplate steps: extract `JsValue`, convert to `JsObject`, extract next
value, etc.  It would be nice to have DSL for traversing through the objects.
I think it can be similar to XPath:

```Scala
entity(as[Person]) { person =>
  if (person / "address" / "city" === "Moscow") {
    complete(StatusCodes.Ok)
  } else {
    complete(StatusCodes.BadRequest)
  }
}
```

Let's create implicit class to extend `JsObject`:

```Scala
implicit class JsObjectOps(val o: JsObject) extends AnyVal {
  def / (name: String) = ???
}
```

But what should it return? We can return `JsValue`, but there are several problems:

1. The object might not contain the field we are looking for. So, we need at
   least `Option[JsValue]`.
2. We'd like to chain path elements, to create more complex paths.
3. We need to have `===` and `=!=` operators to check returned values.

To meet all of these requirements, we need to create another class, which will
wrap `Option[JsValue]`:

```Scala
class JsFieldOps(val field: Option[JsValue]) {
  def /(name: string) = field map (_ / name) getOrElse this
  def ===(x: JsValue) = field.contains(x)
  def =!=(x: JsValue) = !field.contains(x)
}
```

The `===` and `=!=` are quite obvious. We just check values in the underlying
field.  Most interesting part is the `/` method (there is no rocket science as
well :)).  There are two cases.  If the field is empty we can just return the same
empty object.  But if it not,  we can apply the same `/` we used initially to
create this object (that's about the part I skipped not implemented yet).  So it
looks like we need to extend `JsValue`, not `JsObject` to add `/`, but it have
to be applicable only to objects.  There is a method called `asJsObject` in
`JsObject` class, which throws an exception if class is not `JsObject`.  Thus,
the implementation of `JsValueOps` (instead of `JsObjectOps`) will be like:

```Scala
import scala.util.Try
import spray.json.JsValue

implicit class JsValueOps(val value: JsValue) extends AnyVal {
  def /(name: String) =
  JsFieldOps(Try(value.asJsObject).toOption.flatMap(_.fields.get(name)))
}
```

And this is a complete implementation of simple DSL for querying values in
`JsObject`s.

P.S. there is a great library called
[json-lenses](https://github.com/jrudolph/json-lenses) which provides more
powerful way to query and update JSON objects.  It gives you objects called
"lens", which encapsulate path through JSON object, and allows you to get and set
values (of course set means create a modified object, because `JsObject` is
immutable).
