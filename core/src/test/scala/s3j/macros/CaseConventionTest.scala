package s3j.macros

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import s3j.macros.generic.CaseConvention

class CaseConventionTest extends AnyFlatSpec with Matchers {
  it should "split names into words" in {
    CaseConvention.parseWords("meow") shouldBe Seq("meow")
    CaseConvention.parseWords("somethingUseful") shouldBe Seq("something", "useful")
    CaseConvention.parseWords("I_AM_SCREAMING") shouldBe Seq("i", "am", "screaming")
    CaseConvention.parseWords("FBIIsWatchingYou") shouldBe Seq("fbiis", "watching", "you")
    CaseConvention.parseWords("FBI_IsWatchingYou") shouldBe Seq("fbi", "is", "watching", "you")
  }

  it should "convert to camel case" in {
    CaseConvention.CamelCase.transform("MeowOink") shouldBe "meowOink"
    CaseConvention.CamelCase.transform("I_AM_MEOWING") shouldBe "iAmMeowing"
  }

  it should "convert to snake case" in {
    CaseConvention.SnakeCase.transform("MeowOink") shouldBe "meow_oink"
    CaseConvention.SnakeCase.transform("I_AM_MEOWING") shouldBe "i_am_meowing"
  }

  it should "convert to screaming snake case" in {
    CaseConvention.ScreamingSnakeCase.transform("screamingSnake") shouldBe "SCREAMING_SNAKE"
    CaseConvention.ScreamingSnakeCase.transform("quick_brown_fox") shouldBe "QUICK_BROWN_FOX"
  }

  it should "convert to kebab case" in {
    CaseConvention.KebabCase.transform("kebabCase") shouldBe "kebab-case"
    CaseConvention.KebabCase.transform("TEST_CONV") shouldBe "test-conv"
  }

  it should "convert to capitalized kebab case" in {
    CaseConvention.CapitalizedKebabCase.transform("kebabCase") shouldBe "Kebab-Case"
    CaseConvention.CapitalizedKebabCase.transform("TEST_CONV") shouldBe "Test-Conv"
  }

  it should "convert to pascal case" in {
    CaseConvention.PascalCase.transform("pascalCase") shouldBe "PascalCase"
    CaseConvention.PascalCase.transform("some_string") shouldBe "SomeString"
  }

  it should "handle special conventions" in {
    CaseConvention.NoConvention.transform("XyZwAbCd") shouldBe "XyZwAbCd"
  }
}
