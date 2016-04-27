package com.wix.pay.worldpay


import com.wix.pay.worldpay.parsers.JsonWorldpayAuthorizationParser
import org.specs2.mutable.SpecWithJUnit


/**
 * * @author <a href="mailto:lidanh@wix.com">Lidan Hifi</a>
 */
class JsonWorldpayAuthorizationParserTest extends SpecWithJUnit {
  val parser = new JsonWorldpayAuthorizationParser
  val orderCode = "123"
  val orderCurrency = "USD"

  "stringify and then parse" should {
    "return an order similar to the original one" in {
      val authorization = WorldpayAuthorization(orderCode, orderCurrency)
      val authorizationKey = parser.stringify(authorization)

      parser.parse(authorizationKey) must be_==(authorization)
    }
  }
}
