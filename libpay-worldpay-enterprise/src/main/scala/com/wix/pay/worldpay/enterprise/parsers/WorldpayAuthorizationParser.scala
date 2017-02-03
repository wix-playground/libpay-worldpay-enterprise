package com.wix.pay.worldpay.enterprise.parsers

import com.wix.pay.worldpay.enterprise.WorldpayAuthorization

trait WorldpayAuthorizationParser {
  def parse(authorizationKey: String): WorldpayAuthorization
  def stringify(authorization: WorldpayAuthorization): String
}