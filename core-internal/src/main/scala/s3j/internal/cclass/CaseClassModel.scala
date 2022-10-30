package s3j.internal.cclass

import s3j.internal.cclass.CaseClassModel.{DecoderCode, DecodingEnv}
import s3j.internal.utils.Variable
import s3j.io.{JsonReader, JsonWriter, KeyHandle}

import scala.quoted.{Expr, Quotes, Type}

object CaseClassModel {
  trait DecoderCode[T] {
    def variables: Seq[Variable[_]]

    def staticKey(key: String)(using Quotes): Expr[Any]
    def dynamicKey(key: Expr[KeyHandle])(using Quotes): Expr[Any]
    def finalCode()(using Quotes): Expr[Any]
    def result()(using Quotes): Expr[T]
  }
  
  trait DecodingEnv {
    def isKeyPresent(key: String): Expr[Boolean]
  }
}

trait CaseClassModel[T] {
  def keys: Set[String]
  def dynamicKeys: Boolean
  def encode(writer: Expr[JsonWriter], value: Expr[T])(using Quotes): Expr[Any]
  def decode(reader: Expr[JsonReader])(using Quotes, DecodingEnv): DecoderCode[T]
}
