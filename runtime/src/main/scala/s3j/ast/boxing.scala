package s3j.ast

import s3j.ast.{JsBoolean, JsNumber, JsString}

given jsBooleanBox: Conversion[Boolean, JsBoolean] = JsBoolean.apply _
given jsBooleanUnbox: Conversion[JsBoolean, Boolean] = _.value

given jsNumberBoxInt: Conversion[Int, JsNumber] = JsNumber.apply(_)
given jsNumberBoxLong: Conversion[Long, JsNumber] = JsNumber.apply(_)
given jsNumberBoxFloat: Conversion[Float, JsNumber] = JsNumber.apply(_)
given jsNumberBoxDouble: Conversion[Double, JsNumber] = JsNumber.apply(_)
given jsNumberBoxBigInt: Conversion[BigInt, JsNumber] = JsNumber.apply(_)
given jsNumberBoxBigDecimal: Conversion[BigDecimal, JsNumber] = JsNumber.apply(_)

given jsNumberUnboxInt: Conversion[JsNumber, Int] = _.toInt
given jsNumberUnboxLong: Conversion[JsNumber, Long] = _.toLong
given jsNumberUnboxFloat: Conversion[JsNumber, Float] = _.toFloat
given jsNumberUnboxDouble: Conversion[JsNumber, Double] = _.toDouble
given jsNumberUnboxBigInt: Conversion[JsNumber, BigInt] = _.toBigInt
given jsNumberUnboxBigDecimal: Conversion[JsNumber, BigDecimal] = _.toBigDecimal

given jsStringBox: Conversion[String, JsString] = JsString.apply _
given jsStringUnbox: Conversion[JsString, String] = _.value
