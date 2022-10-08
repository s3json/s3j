package s3j.macros

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import s3j.annotations.naming.CaseConvention

class CaseConventionTest extends AnyFlatSpec with Matchers {
  it should "split names into words" in {
    CaseConvention.parseWords("meow") shouldBe Seq("meow")
    CaseConvention.parseWords("somethingUseful") shouldBe Seq("something", "useful")
    CaseConvention.parseWords("I_AM_SCREAMING") shouldBe Seq("i", "am", "screaming")
    CaseConvention.parseWords("FBIIsWatchingYou") shouldBe Seq("fbiis", "watching", "you")
    CaseConvention.parseWords("FBI_IsWatchingYou") shouldBe Seq("fbi", "is", "watching", "you")
  }

  it should "convert to camel case" in {
    CaseConvention.transform(CaseConvention.CamelCase, "MeowOink") shouldBe "meowOink"
    CaseConvention.transform(CaseConvention.CamelCase, "I_AM_MEOWING") shouldBe "iAmMeowing"
  }

  it should "convert to snake case" in {
    CaseConvention.transform(CaseConvention.SnakeCase, "MeowOink") shouldBe "meow_oink"
    CaseConvention.transform(CaseConvention.SnakeCase, "I_AM_MEOWING") shouldBe "i_am_meowing"
  }

  it should "convert to screaming snake case" in {
    CaseConvention.transform(CaseConvention.ScreamingSnakeCase, "screamingSnake") shouldBe "SCREAMING_SNAKE"
    CaseConvention.transform(CaseConvention.ScreamingSnakeCase, "quick_brown_fox") shouldBe "QUICK_BROWN_FOX"
  }

  it should "convert to kebab case" in {
    CaseConvention.transform(CaseConvention.KebabCase, "kebabCase") shouldBe "kebab-case"
    CaseConvention.transform(CaseConvention.KebabCase, "TEST_CONV") shouldBe "test-conv"
  }

  it should "convert to capitalized kebab case" in {
    CaseConvention.transform(CaseConvention.CapitalizedKebabCase, "kebabCase") shouldBe "Kebab-Case"
    CaseConvention.transform(CaseConvention.CapitalizedKebabCase, "TEST_CONV") shouldBe "Test-Conv"
  }

  it should "convert to pascal case" in {
    CaseConvention.transform(CaseConvention.PascalCase, "pascalCase") shouldBe "PascalCase"
    CaseConvention.transform(CaseConvention.PascalCase, "some_string") shouldBe "SomeString"
  }

  it should "handle special conventions" in {
    CaseConvention.transform(CaseConvention.Custom(_ => "out"), "in") shouldBe "out"
    CaseConvention.transform(CaseConvention.NoConvention, "XyZwAbCd") shouldBe "XyZwAbCd"
  }
}
