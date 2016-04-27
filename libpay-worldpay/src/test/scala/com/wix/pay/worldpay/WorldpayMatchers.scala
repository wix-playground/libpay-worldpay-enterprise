package com.wix.pay.worldpay


import com.wix.pay.creditcard.CreditCard
import org.specs2.matcher.MustMatchers._
import org.specs2.matcher._
import org.specs2.mutable.SpecWithJUnit
import spray.http.HttpRequest

import scala.collection.mutable.Buffer
import scala.xml.{Node, NodeSeq, XML}


trait WorldpayMatchers { this: SpecWithJUnit =>
  def beAValidPaymentDetailsFor(cardNode: String, creditCard: CreditCard): Matcher[Node] = {
    not(beEmpty[NodeSeq]) ^^ { elem: Node => (elem \\ "paymentDetails" \ cardNode) aka "card type" } and
      be_==(creditCard.number) ^^ { elem: Node => (elem \\ "cardNumber").text } and
      be_==(creditCard.expiration.month.toString) ^^ { elem: Node => (elem \\ "date" \ "@month").text } and
      be_==(creditCard.expiration.year.toString) ^^ { elem: Node => (elem \\ "date" \ "@year").text } and
      be_==(creditCard.holderName.getOrElse("")) ^^ { elem: Node => (elem \\ "cardHolderName").text } and
      (if (creditCard.csc.isDefined) {
        be_==(creditCard.csc.get) ^^ { elem: Node => (elem \\ "cvc").text }
      } else {
        beEmpty[NodeSeq] ^^ { elem: Node => elem \\ "cvc" }
      })
  }

  def beAValidCurrencyAmount(currency: String, amount: Int, exponent: Int): Matcher[Node] = {
    beAValidCurrencyAmount(currency, amount) and
      be_==(exponent.toString) ^^ { node: Node => (node \\ "amount" \ "@exponent").text }
  }

  def beAValidCurrencyAmount(currency: String, amount: Int): Matcher[Node] = {
    be_==(currency) ^^ { node: Node => (node \\ "amount" \ "@currencyCode").text } and
      be_==(amount.toString) ^^ { node: Node => (node \\ "amount" \ "@value").text }
  }

  def beAValidAuthorizationRequest(merchantKey: Matcher[String] = AlwaysMatcher(),
                                   paymentDetails: Matcher[Node] = AlwaysMatcher(),
                                   currencyAmount: Matcher[Node] = AlwaysMatcher()): Matcher[Node] = {
    merchantKey ^^ { elem: Node => (elem \\ "paymentService" \ "@merchantCode").text } and
      not(beEmpty[NodeSeq]) ^^ { elem: Node => elem \\ "order" \ "@orderCode" aka "order code" } and
      paymentDetails ^^ { elem: Node => (elem \\ "paymentDetails").head aka "payment details" } and
      currencyAmount ^^ { elem: Node => (elem \\ "amount").head }
  }

  def beAValidModificationRequest(merchantKey: String,
                                  orderCode: String,
                                  modificationType: String,
                                  modification: Matcher[Node] = AlwaysMatcher()): Matcher[Node] = {
    be_==(merchantKey) ^^ { elem: Node => (elem \\ "paymentService" \ "@merchantCode").text } and
      be_==(orderCode) ^^ { elem: Node => (elem \\ "orderModification" \ "@orderCode").text } and
      not(beEmpty[NodeSeq]) ^^ { elem: Node => elem \\ modificationType aka "modification type" } and
      modification ^^ { elem: Node => (elem \\ modificationType).head aka "modification details" }
  }

  def containsASaleTransactionFor(merchantKey: String,
                                 orderCode: String,
                                 currency: String,
                                 amount: Int,
                                 cardNode: String,
                                 creditCard: CreditCard): Matcher[Buffer[HttpRequest]] = {
    // exactly 2 requests were sent
    haveSize[Buffer[_]](2) and
    // the first request should be authorization
    beAValidAuthorizationRequest(merchantKey = ===(merchantKey),
                                 paymentDetails = beAValidPaymentDetailsFor(cardNode, creditCard),
                                 currencyAmount = beAValidCurrencyAmount(currency, amount)) ^^ { req: Buffer[HttpRequest] => XML.loadString(req.head.entity.asString) aka "Authorization request" } and
    // the following should be capture
    beAValidModificationRequest(merchantKey = merchantKey,
                                orderCode = orderCode,
                                modificationType = "capture",
                                modification = beAValidCurrencyAmount(currency, amount)) ^^ { req: Buffer[HttpRequest] => XML.loadString(req(1).entity.asString) aka "Capture request" }
  }
}
