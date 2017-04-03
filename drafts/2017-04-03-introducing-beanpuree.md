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
}
```


