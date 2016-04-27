package com.wix.pay.worldpay


import java.math.{BigDecimal => JBigDecimal}

import com.wix.pay.PaymentErrorException
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.creditcard.networks.Networks
import com.wix.pay.model.{CurrencyAmount, Deal}

import scala.xml.{Elem, Node, NodeSeq}


object WorldpayGatewayHelper {
  private val defaultExponent = 2
  private val exceptionalExponents = Map(
    "CLP" -> 0,
    "ISK" -> 0,
    "JPY" -> 0,
    "KRW" -> 0,
    "VND" -> 0)

  private def exponentFor(currency: String): Int = exceptionalExponents.getOrElse(currency, defaultExponent)

  def createAuthorizationRequest(merchantKey: String, creditCard: CreditCard, currencyAmount: CurrencyAmount, deal: Deal): Elem = {
    val exponent = WorldpayGatewayHelper.exponentFor(currencyAmount.currency)
    val amount = toWorldpayAmount(currencyAmount)

    <paymentService version="1.0" merchantCode={merchantKey}>
      <submit>
        <order orderCode={deal.id}>
          <description>{s"${deal.description.getOrElse("")}${deal.invoiceId.fold("")(i => s" ($i)")}"}</description>
          <amount currencyCode={currencyAmount.currency} exponent={exponent.toString} value={amount.toString} />
          {createPaymentDetails(creditCard)}
        </order>
      </submit>
    </paymentService>
  }

  private def createPaymentDetails(creditCard: CreditCard): Node = {
    Networks(creditCard.number) match {
      case Some(network) => network match {
        case SupportedCreditCard(supportedNetwork) =>
          <paymentDetails>
            {
            supportedNetwork.copy(child =
              <cardNumber>{creditCard.number}</cardNumber> ++
                <expiryDate> <date month={creditCard.expiration.month.toString} year={creditCard.expiration.year.toString} /></expiryDate> ++
                <cardHolderName>{creditCard.holderName.getOrElse("Unknown")}</cardHolderName> ++
                {creditCard.csc.fold(NodeSeq.Empty)(cvc => <cvc>{cvc}</cvc>)})
            }
          </paymentDetails>

        case _ => throw new PaymentErrorException(s"Unsupported credit card network: $network")
      }
      case None => throw new PaymentErrorException("Unknown credit card network")
    }
  }

  private def modificationRequest(merchantKey: String, orderCode: String)(modification: Node): Node = {
    <paymentService version="1.4" merchantCode={merchantKey}>
      <modify>
        <orderModification orderCode={orderCode}>
          {modification}
        </orderModification>
      </modify>
    </paymentService>
  }

  def createCaptureRequest(merchantKey: String, orderCode: String, currencyAmount: CurrencyAmount): Node = {
    val exponent = WorldpayGatewayHelper.exponentFor(currencyAmount.currency)
    val amount = toWorldpayAmount(currencyAmount)

    modificationRequest(merchantKey, orderCode) {
      <capture>
        <amount currencyCode={currencyAmount.currency} exponent={exponent.toString} value={amount.toString} />
      </capture>
    }
  }

  def createRefundRequest(merchantKey: String, orderCode: String, currencyAmount: CurrencyAmount): Node = {
    val exponent = WorldpayGatewayHelper.exponentFor(currencyAmount.currency)
    val amount = toWorldpayAmount(currencyAmount)

    modificationRequest(merchantKey, orderCode) {
      <refund>
        <amount currencyCode={currencyAmount.currency} exponent={exponent.toString} value={amount.toString} />
      </refund>
    }
  }

  def createVoidAuthorizationRequest(merchantKey: String, orderCode: String) = {
    modificationRequest(merchantKey, orderCode) {
        <cancel />
    }
  }

  def createVoidRequest(merchantKey: String, orderCode: String) = {
    modificationRequest(merchantKey, orderCode) {
        <cancelOrRefund />
    }
  }

  def toWorldpayAmount(currencyAmount: CurrencyAmount): Int = {
    val exponent = WorldpayGatewayHelper.exponentFor(currencyAmount.currency)

    JBigDecimal.valueOf(currencyAmount.amount).movePointRight(exponent).intValueExact()
  }
}
