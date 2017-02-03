package com.wix.pay.worldpay.enterprise

import com.wix.pay.worldpay.enterprise.parsers.JsonWorldpayEnterpriseMerchantParser
import org.specs2.mutable.SpecWithJUnit


/**
 * * @author <a href="mailto:lidanh@wix.com">Lidan Hifi</a>
 */
class JsonWorldpayEnterpriseMerchantParserTest extends SpecWithJUnit {
  val parser = new JsonWorldpayEnterpriseMerchantParser
  val merchantCode = "someMerchant"
  val merchantPassword = "somePass"

  "stringify and then parse" should {
    "return an order similar to the original one" in {
      val merchant = WorldpayEnterpriseMerchant(merchantCode, merchantPassword)
      val merchantKey = parser.stringify(merchant)

      parser.parse(merchantKey) must be_==(merchant)
    }
  }
}
