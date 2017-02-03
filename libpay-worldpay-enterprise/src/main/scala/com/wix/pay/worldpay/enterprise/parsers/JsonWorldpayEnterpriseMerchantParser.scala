package com.wix.pay.worldpay.enterprise.parsers

import com.wix.pay.worldpay.enterprise.WorldpayEnterpriseMerchant
import org.json4s.DefaultFormats
import org.json4s.native.Serialization

class JsonWorldpayEnterpriseMerchantParser extends WorldpayEnterpriseMerchantParser {
  private implicit val formats = DefaultFormats

  def parse(merchantKey: String): WorldpayEnterpriseMerchant = {
    Serialization.read[WorldpayEnterpriseMerchant](merchantKey)
  }

  def stringify(merchant: WorldpayEnterpriseMerchant): String = {
    Serialization.write(merchant)
  }
}
