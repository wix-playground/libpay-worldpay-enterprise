package com.wix.pay.worldpay


import java.io.StringWriter

import akka.actor.ActorSystem
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.creditcard.networks.Networks
import com.wix.pay.model.{CurrencyAmount, Customer, Deal}
import com.wix.pay.worldpay.parsers._
import com.wix.pay.{PaymentErrorException, PaymentException, PaymentGateway, PaymentRejectedException}
import spray.client.pipelining.sendReceive
import spray.http._
import spray.httpx.RequestBuilding._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}
import scala.xml.{Elem, Node, XML}


object Endpoints {
  val production = "https://secure.worldpay.com/jsp/merchant/xml/paymentService.jsp"
  val test = "https://secure-test.worldpay.com/jsp/merchant/xml/paymentService.jsp"
}

/** A subclass of the [[PaymentGateway]], for Worldpay gateway.
  *
  * @param endpointUrl
  *                The endpoint URL for submitting payment requests. Worldpay API is based on a single path.
  *
 * * @author <a href="mailto:lidanh@wix.com">Lidan Hifi</a>
 */
class WorldpayGateway(endpointUrl: String = Endpoints.production,
                       orderParser: WorldpayAuthorizationParser = new JsonWorldpayAuthorizationParser,
                       merchantParser: WorldpayMerchantParser = new JsonWorldpayMerchantParser) extends PaymentGateway {
  implicit val system = ActorSystem()
  import system.dispatcher

  private def postRequest(merchant: WorldpayMerchant, requestXml: Node): HttpResponse = {
    val w = new StringWriter()
    XML.write(w, requestXml, "UTF-8", xmlDecl = true, WorldpayGateway.WORLDPAY_DTD)

    val pipeline: HttpRequest => Future[HttpResponse] = (
      addCredentials(BasicHttpCredentials(merchant.merchantCode, merchant.merchantPassword))    // add Authorization header with base-64 encoded credentials
        ~> sendReceive)
    val futureResponse = pipeline(Post(endpointUrl, w.toString))
    Await.result(futureResponse, 5.seconds)
  }

  private def withTry(request: => String): Try[String] = {
    Try { request } match {
      case Failure(e) if !e.isInstanceOf[PaymentException] => Failure(PaymentErrorException(cause = e))
      case otherwise => otherwise
    }
  }

  override def authorize(merchantKey: String, creditCard: CreditCard, currencyAmount: CurrencyAmount, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    require(deal.isDefined, "Deal is mandatory for Worldpay")

    withTry {
      val merchant = merchantParser.parse(merchantKey)
      val response = postRequest(merchant, WorldpayGatewayHelper.createAuthorizationRequest(merchant.merchantCode, creditCard, currencyAmount, deal.get))

      response match {
        case WasAuthorizedSuccessfully(orderId) => orderParser.stringify(WorldpayAuthorization(orderId, currencyAmount.currency))
        case AuthorizationFailed(event, code, description) if event == "REFUSED" => throw new PaymentRejectedException(s"Error code: $code, Error message: $description")
        case AuthorizationFailed(event, code, description) if event == "ERROR" => throw new PaymentErrorException(s"Error code: $code, Error message: $description")
        case res => throw new PaymentErrorException(s"Worldpay server returned ${res.status.intValue}: ${res.entity.asString}")
      }
    }
  }

  override def voidAuthorization(merchantKey: String, authorizationKey: String): Try[String] = {
    // Cancel a payment after it has reached the status AUTHORISED.
    // It can no longer be cancelled after it has reached the status of CAPTURED.
    withTry {
      val merchant = merchantParser.parse(merchantKey)
      val order = orderParser.parse(authorizationKey)
      val response = postRequest(merchant, WorldpayGatewayHelper.createVoidAuthorizationRequest(merchant.merchantCode, order.orderCode))

      response match {
        case WasCancelledSuccessfully(orderCode) => orderParser.stringify(WorldpayAuthorization(orderCode, order.currency))
        case ModificationFailed(errorCode, errorDescription) => throw new PaymentErrorException(s"Error code: $errorCode, Error message: $errorDescription")
        case res => throw new PaymentErrorException(s"Worldpay server returned ${res.status.intValue}: ${res.entity.asString}")
      }
    }
  }

  override def sale(merchantKey: String, creditCard: CreditCard, currencyAmount: CurrencyAmount, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    val authorizationResult = authorize(merchantKey, creditCard, currencyAmount, customer, deal)

    authorizationResult match {
      case Success(orderCode) => capture(merchantKey, orderCode, currencyAmount.amount)
      case _ => authorizationResult
    }
  }

  override def capture(merchantKey: String, authorizationKey: String, amount: Double): Try[String] = {
    withTry {
      val merchant = merchantParser.parse(merchantKey)
      val order = orderParser.parse(authorizationKey)
      val response = postRequest(merchant, WorldpayGatewayHelper.createCaptureRequest(merchant.merchantCode, order.orderCode, CurrencyAmount(order.currency, amount)))

      response match {
        case WasCapturedSuccessfully(orderCode, currency) => orderParser.stringify(WorldpayAuthorization(orderCode, currency))
        case ModificationFailed(errorCode, errorDescription) => throw new PaymentErrorException(s"Error code: $errorCode, Error message: $errorDescription")
        case res => throw new PaymentErrorException(s"Worldpay server returned ${res.status.intValue}: ${res.entity.asString}")
      }
    }
  }
}

object HasXmlResponse {
  def unapply(response: HttpResponse): Option[Elem] = {
    Try(XML.loadString(response.entity.asString)).toOption
  }
}

object WasAuthorizedSuccessfully {
  def unapply(response: HttpResponse): Option[String] = {
    response match {
      case HasXmlResponse(elem) if (elem \\ "lastEvent").text == "AUTHORISED" => Some((elem \\ "orderStatus" \ "@orderCode").text)
      case _ => None
    }
  }
}

object AuthorizationFailed {
  def unapply(response: HttpResponse): Option[(String, String, String)] = {
    response match {
      case HasXmlResponse(elem) if requestFailed(elem) => Some((elem \\ "lastEvent").text,
                                                                (elem \\ "ISO8583ReturnCode" \ "@code").text,
                                                                (elem \\ "ISO8583ReturnCode" \ "@description").text)
      case HasXmlResponse(elem) if (elem \\ "error").nonEmpty => Some("ERROR", (elem \\ "error" \ "@code").text,
                                                                      (elem \\ "error").text)
      case _ => None
    }
  }

  private def requestFailed(elem: Elem): Boolean = {
    (elem \\ "lastEvent").text != "AUTHORISED" && (elem \\ "ISO8583ReturnCode").nonEmpty
  }
}

object SupportedCreditCard {
  def unapply(network: String): Option[Elem] = {
    network match {
      case Networks.visa => Some(<VISA-SSL></VISA-SSL>)
      case Networks.masterCard => Some(<ECMC-SSL></ECMC-SSL>)
      case Networks.amex => Some(<AMEX-SSL></AMEX-SSL>)
      case Networks.diners => Some(<DINERS-SSL></DINERS-SSL>)
      case Networks.discover => Some(<DISCOVER-SSL></DISCOVER-SSL>)
      case Networks.maestro => Some(<MAESTRO-SSL></MAESTRO-SSL>)
      case _ => None
    }
  }
}

object WasCapturedSuccessfully {
  def unapply(response: HttpResponse): Option[(String, String)] = {
    response match {
      case HasXmlResponse(elem) if (elem \\ "captureReceived").nonEmpty =>
        Some((elem \\ "captureReceived" \ "@orderCode").text, (elem \\ "amount" \ "@currencyCode").text)
      case _ => None
    }
  }
}

object WasCancelledSuccessfully {
  def unapply(response: HttpResponse): Option[String] = {
    response match {
      case HasXmlResponse(elem) if (elem \\ "cancelReceived").nonEmpty => Some((elem \\ "cancelReceived" \ "@orderCode").text)
      case _ => None
    }
  }
}

object ModificationFailed {
  def unapply(response: HttpResponse): Option[(String, String)] = {
    response match {
      case HasXmlResponse(elem) if (elem \\ "error").nonEmpty => Some((elem \\ "error" \ "@code").text, (elem \\ "error").text)
      case _ => None
    }
  }
}

object WorldpayGateway {
  private val WORLDPAY_DTD = xml.dtd.DocType("paymentService", xml.dtd.PublicID("-//WorldPay//DTD WorldPay PaymentService v1//EN", "http://dtd.worldpay.com/paymentService_v1.dtd"), Nil)
}
