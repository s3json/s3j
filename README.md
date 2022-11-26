s3j
===

**s3j** is a general-purpose JSON library for Scala 3, heavily focused on macros.

## Goals
As JSON is a data exchange format, we define the term '*general purpose*' as 'being able to easily interact with most remote systems', adapting to their serialization formats. Accordingly, this library specifies the following goals:

* **Safety**, mainly concerning the resilience to DoS attacks. It should not be possible for specially crafted inputs to cause significant performance degradation.
* **Usability**: there is no such thing as 'canonical JSON encoding'; every service is different. In some cases, JSON trees are more convenient than case classes. Sometimes case class has an untyped piece. More complex projects may require more fine-grained control over JSON serialization. Both the runtime code and the macro engine should be able to handle all of this.
* **Composability**: when some abstraction fails, it should be possible to dig one level down and implement your idea.
* **Performance**: the library should deliver the best possible performance at chosen abstraction level. However, it's not a goal to be the fastest library in the world and to count every femtosecond. Every abstraction comes with a price.

## Usage

Full documentation is located at **&lt;TODO LINK HERE&gt;**. Here is just a quick example of library usage:

First, enable the library in your project:
```scala
// project/plugins.sbt
addSbtPlugin("io.s3j" % "s3j-sbt" % "VERSION")


// build.sbt
enablePlugins(S3jPlugin)
```

Then, use it in your code:
```scala
import s3j.*

object Test extends App {
  case class Foo(x: Int, y: String, z: Boolean) derives JsonFormat

  println(Foo(123, "test", z = true).toJsonString)
  // => {"x":123,"y":"test","z":true}

  println("""{ "x": 456, "y": "foo", "z": false }""".fromJson[Foo])
  // => Foo(456,foo,false)
}
```
