package com.wix.pay.worldpay.enterprise.parsers

import com.wix.pay.worldpay.enterprise.WorldpayEnterpriseAuthorization

trait WorldpayEnterpriseAuthorizationParser {
  def parse(authorizationKey: String): WorldpayEnterpriseAuthorization
  def stringify(authorization: WorldpayEnterpriseAuthorization): String
}