package com.wix.pay.worldpay.enterprise.testkit


import scala.xml.{Node, PCData}
import java.math.{BigDecimal => JBigDecimal}
import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.model._
import com.google.api.client.util.Base64
import com.wix.e2e.http.HttpRequest
import com.wix.e2e.http.api.StubWebServer
import com.wix.e2e.http.client.extractors.HttpMessageExtractors._
import com.wix.e2e.http.server.WebServerFactory.aStubWebServer
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.{CurrencyAmount, Customer, Deal}


class WorldpayEnterpriseDriver(port: Int) {
  private val server: StubWebServer = aStubWebServer.onPort(port).build

  def start(): Unit = server.start()
  def stop(): Unit = server.stop()
  def reset(): Unit = {
    server.replaceWith()
    server.clearRecordedRequests()
  }

  def requests: Seq[HttpRequest] = server.recordedRequests

  def anAuthorizationRequest(merchantCode: String,
                             merchantPassword: String,
                             creditCard: CreditCard,
                             currencyAmount: CurrencyAmount,
                             customer: Option[Customer],
                             deal: Option[Deal]): AuthorizationRequest = {
    AuthorizationRequest(merchantCode, merchantPassword, creditCard, currencyAmount, customer, deal)
  }

  def aCaptureRequest(merchantCode: String,
                      merchantPassword: String,
                      orderCode: String,
                      currencyAmount: CurrencyAmount,
                      exponent: Int = 2): CaptureRequest = {
    CaptureRequest(merchantCode, merchantPassword, orderCode, currencyAmount, exponent)
  }

  def aRefundRequest(merchantCode: String,
                     merchantPassword: String,
                     orderCode: String,
                     currencyAmount: CurrencyAmount,
                     exponent: Int = 2): RefundRequest = {
    RefundRequest(merchantCode, merchantPassword, orderCode, currencyAmount, exponent)
  }

  def aVoidAuthorizationRequest(merchantCode: String,
                                merchantPassword: String,
                                orderCode: String): VoidAuthorizationRequest = {
    VoidAuthorizationRequest(merchantCode, merchantPassword, orderCode)
  }

  def aVoidRequest(merchantCode: String,
                   merchantPassword: String,
                   orderCode: String): VoidRequest = {
    VoidRequest(merchantCode, merchantPassword, orderCode)
  }

  abstract class WorldpayRequest(merchantCode: String,
                                 merchantPassword: String) {
    protected def isStubbedEntity(entity: HttpEntity): Boolean
    protected def validResponse(orderCode: String): Node
    protected def invalidResponse(event: Option[String] = None, errorCode: String, errorDescription: String): Node

    private def isValidWorldPayXML(entity: HttpEntity): Boolean = {
      entity.extractAsString.startsWith(
        """<?xml version='1.0' encoding='UTF-8'?>
          |<!DOCTYPE paymentService PUBLIC "-//WorldPay//DTD WorldPay PaymentService v1//EN" "http://dtd.worldpay.com/paymentService_v1.dtd">"""
          .stripMargin)
    }

    private def isAuthorized(headers: Seq[HttpHeader]): Boolean = {
      val expectedValue = s"Basic ${Base64.encodeBase64String(s"$merchantCode:$merchantPassword".getBytes("UTF-8"))}"
      headers.exists( h => h.name == "Authorization" && h.value == expectedValue)
    }

    protected def hasAmountNode(node: Node, currencyAmount: CurrencyAmount, exponent: Int): Boolean = {
      (node \\ "amount" \ "@currencyCode").text == currencyAmount.currency &&
        (node \\ "amount" \ "@exponent").text == exponent.toString &&
        (node \\ "amount" \ "@value").text ==
          JBigDecimal.valueOf(currencyAmount.amount).movePointRight(exponent).intValueExact().toString
    }

    protected def respondWith(status: StatusCode, content: String): Unit = {
      server.appendAll {
        case HttpRequest(
          HttpMethods.POST,
          Path("/"),
          headers,
          entity, _) if isValidWorldPayXML(entity) && isAuthorized(headers) && isStubbedEntity(entity) =>
            HttpResponse(
              status = status,
              entity = content)
      }
    }

    def returns(orderCode: String): Unit = respondWith(StatusCodes.OK, validResponse(orderCode).toString())

    def errors(errorCode: String, errorDescription: String): Unit = {
      respondWith(StatusCodes.OK, invalidResponse(Some("ERROR"), errorCode, errorDescription).toString())
    }

    def failsWithStatusAndMessage(status: StatusCode, message: String): Unit = respondWith(status, message)
  }

  abstract class WorldpayModificationRequest(orderCode: String,
                                             merchantCode: String,
                                             merchantPassword: String) extends WorldpayRequest(merchantCode, merchantPassword) {
    override protected def invalidResponse(event: Option[String] = None,
                                           errorCode: String,
                                           errorDescription: String): Node = {
      <paymentService version="1.4" merchantCode={merchantCode}>
        <reply>
          <orderStatus orderCode={orderCode}>
            <error code={errorCode}>{PCData(errorDescription)}</error>
          </orderStatus>
        </reply>
      </paymentService>
    }

    protected def isStubbedModification(request: Node): Boolean = {
      (request \\ "paymentService" \ "@merchantCode").text ==
        merchantCode && (request \\ "orderModification" \ "@orderCode").text == orderCode
    }
  }

  case class AuthorizationRequest(merchantCode: String,
                                  merchantPassword: String,
                                  creditCard: CreditCard,
                                  currencyAmount: CurrencyAmount,
                                  customer: Option[Customer],
                                  deal: Option[Deal]) extends WorldpayRequest(merchantCode, merchantPassword) {
    private def response(orderCode: Option[String] = None,
                         lastEvent: String = "AUTHORISED",
                         returnCode: (String, String) = ("0", "AUTHORISED")): Node = {
      <paymentService version="1.4.1" merchantCode={merchantCode}>
        <reply>
          <orderStatus orderCode={orderCode.getOrElse("")}>
            <payment>
              <paymentMethod>VISA-SSL</paymentMethod>
              <amount value="10000" currencyCode="USD" exponent="2" debitCreditIndicator="credit"/>
              <lastEvent>{lastEvent}</lastEvent>
              <CVCResultCode description="APPROVED"/>
              <balance accountType="IN_PROCESS_AUTHORISED">
                <amount value="10000" currencyCode="USD" exponent="2" debitCreditIndicator="credit"/>
              </balance>
              <cardNumber>4444********1111</cardNumber>
              <ISO8583ReturnCode code={returnCode._1} description={returnCode._2} />
              <riskScore value="0"/>
            </payment>
          </orderStatus>
        </reply>
      </paymentService>
    }

    protected override def validResponse(orderCode: String): Node = response(Some(orderCode))

    protected override def isStubbedEntity(entity: HttpEntity): Boolean = {
      val elem = XMLNoDTDValidation.loadString(entity.extractAsString)

      val hasCvvCode = creditCard.csc match {
        case Some(code) => (elem \\ "cvc").text == code
        case None => true
      }

      (elem \\ "paymentService" \ "@merchantCode").text == merchantCode &&
        (elem \\ "cardNumber").text == creditCard.number &&
        (elem \\ "cardHolderName").nonEmpty &&
        hasCvvCode &&
        (elem \\ "expiryDate" \ "date" \ "@month").text == creditCard.expiration.month.toString &&
        (elem \\ "expiryDate" \ "date" \ "@year").text == creditCard.expiration.year.toString &&
        hasAmountNode(elem, currencyAmount, 2)
    }

    protected override def invalidResponse(event: Option[String] = None,
                                           errorCode: String,
                                           errorDescription: String): Node = {
      response(lastEvent = event.getOrElse("ERROR"), returnCode = (errorCode, errorDescription))
    }

    def refuses(errorCode: String, errorDescription: String): Unit = {
      respondWith(StatusCodes.OK, invalidResponse(Some("REFUSED"), errorCode, errorDescription).toString())
    }
  }

  case class CaptureRequest(merchantCode: String,
                            merchantPassword: String,
                            orderCode: String,
                            currencyAmount: CurrencyAmount,
                            exponent: Int) extends WorldpayModificationRequest(orderCode, merchantCode, merchantPassword) {
    def isStubbedEntity(entity: HttpEntity): Boolean = {
      val elem = XMLNoDTDValidation.loadString(entity.extractAsString)

      isStubbedModification(elem) &&
        // capture request
        (elem \\ "capture").nonEmpty &&
        hasAmountNode(elem, currencyAmount, exponent)
    }

    protected override def validResponse(orderCode: String): Node = {
      <paymentService version="1.4" merchantCode={merchantCode}>
        <reply>
          <ok>
            <captureReceived orderCode={orderCode}>
              <amount value={currencyAmount.amount.toString} currencyCode={currencyAmount.currency} exponent={exponent.toString} />
            </captureReceived>
          </ok>
        </reply>
      </paymentService>
    }
  }

  case class RefundRequest(merchantCode: String,
                           merchantPassword: String,
                           orderCode: String,
                           currencyAmount: CurrencyAmount,
                           exponent: Int) extends WorldpayModificationRequest(orderCode, merchantCode, merchantPassword) {
    def isStubbedEntity(entity: HttpEntity): Boolean = {
      val elem = XMLNoDTDValidation.loadString(entity.extractAsString)

      isStubbedModification(elem) &&
        // refund request
        (elem \\ "refund").nonEmpty &&
        hasAmountNode(elem, currencyAmount, exponent)

    }

    protected override def validResponse(orderCode: String): Node = {
      <paymentService version="1.4" merchantCode={merchantCode}>
        <reply>
          <ok>
            <refundReceived orderCode={orderCode}>
              <amount value={currencyAmount.amount.toString} currencyCode={currencyAmount.currency} exponent={exponent.toString} />
            </refundReceived>
          </ok>
        </reply>
      </paymentService>
    }
  }

  case class VoidAuthorizationRequest(merchantCode: String,
                                      merchantPassword: String,
                                      orderCode: String) extends WorldpayModificationRequest(orderCode, merchantCode, merchantPassword) {
    def isStubbedEntity(entity: HttpEntity): Boolean = {
      val elem = XMLNoDTDValidation.loadString(entity.extractAsString)

      isStubbedModification(elem) &&
        // cancel request
        (elem \\ "cancel").nonEmpty
    }

    protected override def validResponse(orderCode: String): Node = {
      <paymentService version="1.4" merchantCode={merchantCode}>
        <reply>
          <ok>
            <cancelReceived orderCode={orderCode} />
          </ok>
        </reply>
      </paymentService>
    }
  }

  case class VoidRequest(merchantCode: String,
                         merchantPassword: String,
                         orderCode: String) extends WorldpayModificationRequest(orderCode, merchantCode, merchantPassword) {
    def isStubbedEntity(entity: HttpEntity): Boolean = {
      val elem = XMLNoDTDValidation.loadString(entity.extractAsString)

      isStubbedModification(elem) &&
        // cancel or refund request
        (elem \\ "cancelOrRefund").nonEmpty
    }

    protected override def validResponse(orderCode: String): Node = {
      <paymentService version="1.4" merchantCode={merchantCode}>
        <reply>
          <ok>
            <voidReceived orderCode={orderCode} />
          </ok>
        </reply>
      </paymentService>
    }
  }
}
