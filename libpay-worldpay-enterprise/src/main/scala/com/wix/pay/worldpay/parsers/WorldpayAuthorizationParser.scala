package com.wix.pay.worldpay.parsers

import com.wix.pay.worldpay.WorldpayAuthorization

trait WorldpayAuthorizationParser {
  def parse(authorizationKey: String): WorldpayAuthorization
  def stringify(authorization: WorldpayAuthorization): String
}