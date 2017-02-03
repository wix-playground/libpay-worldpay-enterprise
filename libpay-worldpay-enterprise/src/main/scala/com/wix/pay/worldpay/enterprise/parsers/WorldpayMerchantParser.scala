package com.wix.pay.worldpay.enterprise.parsers

import com.wix.pay.worldpay.enterprise.WorldpayMerchant

trait WorldpayMerchantParser {
  def parse(authorizationKey: String): WorldpayMerchant
  def stringify(order: WorldpayMerchant): String
}
