package com.wix.pay.worldpay.enterprise

import com.wix.pay.worldpay.enterprise.parsers.JsonWorldpayEnterpriseAuthorizationParser
import org.specs2.mutable.SpecWithJUnit


/**
 * * @author <a href="mailto:lidanh@wix.com">Lidan Hifi</a>
 */
class JsonWorldpayEnterpriseAuthorizationParserTest extends SpecWithJUnit {
  val parser = new JsonWorldpayEnterpriseAuthorizationParser
  val orderCode = "123"
  val orderCurrency = "USD"

  "stringify and then parse" should {
    "return an order similar to the original one" in {
      val authorization = WorldpayEnterpriseAuthorization(orderCode, orderCurrency)
      val authorizationKey = parser.stringify(authorization)

      parser.parse(authorizationKey) must be_==(authorization)
    }
  }
}
