package com.wix.pay.worldpay.enterprise.parsers

import com.wix.pay.worldpay.enterprise.WorldpayEnterpriseAuthorization
import org.json4s.DefaultFormats
import org.json4s.native.Serialization

class JsonWorldpayEnterpriseAuthorizationParser extends WorldpayEnterpriseAuthorizationParser {
  private implicit val formats = DefaultFormats

  def parse(authorizationKey: String): WorldpayEnterpriseAuthorization = {
    Serialization.read[WorldpayEnterpriseAuthorization](authorizationKey)
  }

  def stringify(authorization: WorldpayEnterpriseAuthorization): String = {
    Serialization.write(authorization)
  }
}
