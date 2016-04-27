package com.wix.pay.worldpay.parsers

import com.wix.pay.worldpay.WorldpayMerchant
import org.json4s.DefaultFormats
import org.json4s.native.Serialization

class JsonWorldpayMerchantParser extends WorldpayMerchantParser {
  private implicit val formats = DefaultFormats

  def parse(merchantKey: String): WorldpayMerchant = {
    Serialization.read[WorldpayMerchant](merchantKey)
  }

  def stringify(merchant: WorldpayMerchant): String = {
    Serialization.write(merchant)
  }
}
