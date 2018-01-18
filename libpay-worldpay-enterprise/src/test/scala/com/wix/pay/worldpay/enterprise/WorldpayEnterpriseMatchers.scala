package com.wix.pay.worldpay.enterprise


import akka.http.scaladsl.model.HttpRequest
import com.wix.e2e.http.client.extractors.HttpMessageExtractors._
import com.wix.pay.creditcard.CreditCard
import org.specs2.matcher._
import org.specs2.mutable.SpecWithJUnit

import scala.xml.{Node, NodeSeq, XML}


trait WorldpayEnterpriseMatchers { this: SpecWithJUnit =>
  def beAValidPaymentDetailsFor(cardNode: String, creditCard: CreditCard): Matcher[Node] = {
    not(beEmpty[NodeSeq]) ^^ { elem: Node => elem \\ "paymentDetails" \ cardNode } and
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
      not(beEmpty[NodeSeq]) ^^ { elem: Node => elem \\ "order" \ "@orderCode" } and
      paymentDetails ^^ { elem: Node => (elem \\ "paymentDetails").head } and
      currencyAmount ^^ { elem: Node => (elem \\ "amount").head }
  }

  def beAValidModificationRequest(merchantKey: String,
                                  orderCode: String,
                                  modificationType: String,
                                  modification: Matcher[Node] = AlwaysMatcher()): Matcher[Node] = {
    be_==(merchantKey) ^^ { elem: Node => (elem \\ "paymentService" \ "@merchantCode").text } and
      be_==(orderCode) ^^ { elem: Node => (elem \\ "orderModification" \ "@orderCode").text } and
      not(beEmpty[NodeSeq]) ^^ { elem: Node => elem \\ modificationType } and
      modification ^^ { elem: Node => (elem \\ modificationType).head }
  }

  def containsASaleTransactionFor(merchantKey: String,
                                 orderCode: String,
                                 currency: String,
                                 amount: Int,
                                 cardNode: String,
                                 creditCard: CreditCard): Matcher[Seq[HttpRequest]] = {
    // exactly 2 requests were sent
    haveSize[Seq[_]](2) and
    // the first request should be authorization
    beAValidAuthorizationRequest(
      merchantKey = ===(merchantKey),
      paymentDetails = beAValidPaymentDetailsFor(cardNode, creditCard),
      currencyAmount = beAValidCurrencyAmount(currency, amount)) ^^ { req: Seq[HttpRequest] =>
        XML.loadString(req.head.entity.extractAsString) } and
    // the following should be capture
    beAValidModificationRequest(
      merchantKey = merchantKey,
      orderCode = orderCode,
      modificationType = "capture",
      modification = beAValidCurrencyAmount(currency, amount)) ^^ { req: Seq[HttpRequest] =>
        XML.loadString(req(1).entity.extractAsString) }
  }
}
