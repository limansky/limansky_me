---
title: Make your Java Beans shapeless with BeanPurée
author: Mike Limansky
theme: solarized
---

## Motivation

* Have Java Code
* Want to keep Scala code clean
* Some libraries requires case classes

## Top level API

```Java
public class PersonBean {
    private int age;
    private String name;

    public int getAge() { return age; }
    public void setAge(int age) { this.age = age; }

    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
}
```

```Scala
case class Person(age: Int, name: String)
```

## BeanConverter

```Scala
val converter = BeanConverter[PersonBean, Person]

val p = converter.beanToProduct(bean)

val b = converter.productToBean(p)
```

### More intelligent example

dododo
