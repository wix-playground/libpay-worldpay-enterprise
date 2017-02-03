package com.wix.pay.worldpay.parsers

import com.wix.pay.worldpay.WorldpayMerchant

trait WorldpayMerchantParser {
  def parse(authorizationKey: String): WorldpayMerchant
  def stringify(order: WorldpayMerchant): String
}
