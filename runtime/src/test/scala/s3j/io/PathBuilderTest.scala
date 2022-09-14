package s3j.io

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import s3j.JsPath
import s3j.io.util.{CharRange, JsPathBuilder}

class PathBuilderTest extends AnyFlatSpec with Matchers {
  private def makeBuilder(): JsPathBuilder = new JsPathBuilder(8192, 512)

  extension (b: JsPathBuilder) {
    def appendKey(s: String): Unit = b.appendKey(new CharRange(s.length).put(s).flip())
  }

  it should "work when empty" in {
    makeBuilder().toPath shouldBe JsPath.Root
  }

  it should "preserve empty keys" in {
    val b = makeBuilder()
    b.pushObject()
    b.finishLevel()
    b.toPath shouldBe JsPath.obj("")
  }

  it should "hide not-yet-finished objects" in {
    val b = makeBuilder()
    b.pushObject()
    b.toPath shouldBe JsPath.Root
  }

  it should "hide not-yet-started arrays" in {
    val b = makeBuilder()
    b.pushArray()
    b.toPath shouldBe JsPath.Root
  }

  it should "work in complex scenarios (1)" in {
    val b = makeBuilder()

    b.pushArray()

    b.incrementIndex()
    b.pushObject()
    b.pop() shouldBe JsPathBuilder.RArray
    b.toPath shouldBe JsPath.arr(0)

    b.incrementIndex()
    b.pushObject()
    b.appendKey("x")
    b.finishLevel()
    b.toPath shouldBe JsPath.arr(1).obj("x")

    b.pushArray()

    b.incrementIndex()
    b.pushObject()
    b.pop() shouldBe JsPathBuilder.RArray

    b.incrementIndex()
    b.pushObject()
    b.appendKey("x")
    b.finishLevel()
    b.toPath shouldBe JsPath.arr(1).obj("x").arr(1).obj("x")
  }

  it should "work in complex scenarios (2)" in {
    val b = makeBuilder()

    b.pushArray()
    b.incrementIndex()
    b.toPath shouldBe JsPath.arr(0)

    b.pushObject()
    b.appendKey("x")
    b.finishLevel()
    b.toPath shouldBe JsPath.arr(0).obj("x")

    b.resetKey()
    b.appendKey("y")
    b.finishLevel()
    b.toPath shouldBe JsPath.arr(0).obj("y")

    b.pushArray()
    b.incrementIndex()
    b.incrementIndex()
    b.incrementIndex()
    b.pop() shouldBe JsPathBuilder.RObject

    b.resetKey()
    b.appendKey("z")
    b.finishLevel()
    b.toPath shouldBe JsPath.arr(0).obj("z")

    b.resetKey()
    b.appendKey("w")
    b.finishLevel()
    b.toPath shouldBe JsPath.arr(0).obj("w")

    b.pop() shouldBe JsPathBuilder.RArray
    b.pop() shouldBe JsPathBuilder.RRoot
  }

  it should "work in complex scenarios (3)" in {
    val b = makeBuilder()

    b.pushObject()
    b.appendKey("x")
    b.finishLevel()
    b.toPath shouldBe JsPath.obj("x")

    b.pushObject()
    b.appendKey("x")
    b.finishLevel()
    b.toPath shouldBe JsPath.obj("x").obj("x")

    b.resetKey()
    b.appendKey("y")
    b.finishLevel()
    b.toPath shouldBe JsPath.obj("x").obj("y")
    b.pop() shouldBe JsPathBuilder.RObject

    b.resetKey()
    b.appendKey("y")
    b.finishLevel()
    b.toPath shouldBe JsPath.obj("y")
    b.pop() shouldBe JsPathBuilder.RRoot
  }
}
