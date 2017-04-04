---
title: Introducing BeanPurée
tags: Scala, shapeless, BeanPurée
---

As a Scala developer I prefer to use Scala libraries than Java ones for my
code.  However it is not always possible.  You may not find required library,
or you just have to use some legacy code you already have.  Even though Scala
runs on JVM and fully compatible with Java, the languages have different
ideology, different code style and sometimes different API.  Thus, in Scala we
prefer to use immutable case classes for the data modeling.  However in Java
the common building blocks are JavaBeans, which are mutable.  Another problem
is that a lot of Scala libraries expect case classes.  Even if they work with
JavaBeans, usually you have to write some boilerplate code.

So, quite often it is easier to have separate model in your Scala code, and
converters between Java model classes and Scala ones.

BeanPurée is a library helps you to automate this process.  And you can do even
more, because it's a bridge from JavaBeans to shapeless.

<!--more-->

Let's take this Java class as an example:

```Java
public class Dog {
    private String name;
    private int age;
    private boolean chaseCats;

    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    public int getAge() { return age; }
    public void setAge(int age) { this.age = age; }

    public boolean isChaseCats() { return chaseCats; }
    public void setChaseCats(boolean chaseCats) { this.chaseCats = chaseCats; }

    public String toString() {
        return "Dog " + name + ", " + age +
            (chaseCats ? " is looking for cats" : " is sleeping");
    }
}
```

BeanPurée provides `BeanGeneric` class which has the same function with
shapeless's `Generic`:

```Scala
scala> import me.limansky.beanpuree._
import me.limansky.beanpuree._

scala> val gen = BeanGeneric[Dog]
gen: me.limansky.beanpuree.BeanGeneric[Dog]{type Repr = shapeless.::[String,shapeless.::[Int,shapeless.::[Boolean,shapeless.HNil]]]} = $anon$1@56f5a8b7

```

We just get a `BeanGeneric` instance with representation type `String :: Int ::
Boolean :: HNil`.  Internally `BeanGeneric` uses the getters order to build the
Repr type.  Thus we get this type.  Let's try to use it:

```Scala
scala> val fima = gen.from("Fima" :: 12 :: false :: HNil)
fima: Dog = Dog Fima, 12 is sleeping

scala> fima.setChaseCats(true)

scala> gen.to(fima)
res3: gen.Repr = Fima :: 12 :: true :: HNi
```

So far, so good, let's combine it with shapeless:

```Scala
scala> case class ScalaDog(name: String, age: Int, chaseCats: Boolean)
defined class ScalaDog

scala> val sgen = Generic[ScalaDog]
sgen: shapeless.Generic[ScalaDog]{type Repr = shapeless.::[String,shapeless.::[Int,shapeless.::[Boolean,shapeless.HNil]]]} = anon$macro$8$1@45e06950

scala> sgen.from(gen.to(fima))
res4: ScalaDog = ScalaDog(Fima,12,true)
```

Since the shape of `Dog` and `ScalaDog` is the same we can convert from one
class to another using combination of `Generic` and `BeanGeneric`.

Like shapeless provides `LabelledGeneric` with field names information in the
`Repr` type, BeanPurée provides `LabelledBeanGeneric`, which adds properties
names to generic representation.

```Scala
scala> val lgen = LabelledBeanGeneric[Dog]
lgen: me.limansky.beanpuree.LabelledBeanGeneric[Dog]{type Repr = shapeless.::[String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("name")],String],shapeless.::[Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("age")],Int],shapeless.::[Boolean with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("chaseCats")],Boolean],shapeless.HNil]]]} = me.limansky.beanpuree.LabelledBeanGeneric$$anon$1@412d56b4

scala> lgen.from('name ->> "Rex" :: 'age ->> 5 :: 'chaseCats ->> true :: HNil)
res7: Dog = Dog Rex, 5 is looking for cats
```
