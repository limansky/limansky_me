---
title: Easy JSON analyze with spray-json.
tags: Scala, Akka, Spray, DSL
---

If you are using [Spray](http://spray.io) or [Akka HTTP](http://akka.io) to
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
    addr <- a.fields.get("address")
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

This code is look quite ugly.  Even with two layers we have a lot of
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
