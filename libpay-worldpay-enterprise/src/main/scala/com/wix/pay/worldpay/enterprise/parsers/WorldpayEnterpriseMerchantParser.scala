package com.wix.pay.worldpay.enterprise.parsers

import com.wix.pay.worldpay.enterprise.WorldpayEnterpriseMerchant

trait WorldpayEnterpriseMerchantParser {
  def parse(authorizationKey: String): WorldpayEnterpriseMerchant
  def stringify(order: WorldpayEnterpriseMerchant): String
}
