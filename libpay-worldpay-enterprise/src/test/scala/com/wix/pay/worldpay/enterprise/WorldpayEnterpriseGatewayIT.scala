package com.wix.pay.worldpay.enterprise

import com.wix.pay.creditcard.{CreditCard, CreditCardOptionalFields, YearMonth}
import com.wix.pay.model.{CurrencyAmount, Deal}
import com.wix.pay.worldpay.enterprise.parsers.{JsonWorldpayEnterpriseAuthorizationParser, JsonWorldpayEnterpriseMerchantParser}
import com.wix.pay.worldpay.enterprise.testkit.WorldpayEnterpriseDriver
import com.wix.pay.{PaymentErrorException, PaymentRejectedException}
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope
import spray.http._

import scala.util.Success


/**
 * * @author <a href="mailto:lidanh@wix.com">Lidan Hifi</a>
 */
class WorldpayEnterpriseGatewayIT extends SpecWithJUnit with WorldpayEnterpriseMatchers {
  val probePort = 10001
  val driver = new WorldpayEnterpriseDriver(probePort)
  val authorizationParser = new JsonWorldpayEnterpriseAuthorizationParser
  val merchantParser = new JsonWorldpayEnterpriseMerchantParser
  val someOrderCode = "$$$"
  val merchantCode = "someMerchant"
  val merchantPassword = "somePassword"
  val someValidMerchant = merchantParser.stringify(WorldpayEnterpriseMerchant(merchantCode, merchantPassword))

  val invalidMerchantPassword = ""
  val someInvalidMerchant = merchantParser.stringify(WorldpayEnterpriseMerchant(merchantCode, invalidMerchantPassword  ))

  def anAuthorizationKeyFor(currency: String) = authorizationParser.stringify(
    WorldpayEnterpriseAuthorization(someOrderCode, currency))

  trait Ctx extends Scope {
    val worldpayGateway = new WorldpayEnterpriseGateway(
      s"http://localhost:$probePort",
      orderParser = authorizationParser,
      merchantParser = merchantParser)

    driver.reset()
  }

  step {
    driver.start()
  }

  sequential


  "authorise request" should {
    val creditCard = CreditCard("4580458045804580", YearMonth(2020, 12), None)
    val currencyAmount = CurrencyAmount("USD", 100)
    val deal = Deal("123", Some("title"), Some("desc"))
    val authorizationKey = anAuthorizationKeyFor(currencyAmount.currency)

    "successfully yield an authorization key upon a valid request (no cvv)" in new Ctx {
      driver.anAuthorizationRequest(
        merchantCode, merchantPassword, creditCard, currencyAmount, None, Some(deal)) returns someOrderCode

      worldpayGateway.authorize(someValidMerchant, creditCard, currencyAmount, None, Some(deal)) must
        be_===(Success(authorizationKey))
    }

    "successfully yield an authorization key upon a valid request (with cvv)" in new Ctx {
      val cardWithCvv = creditCard.copy(additionalFields = Some(CreditCardOptionalFields(csc = Some("123"))))

      driver.anAuthorizationRequest(
        merchantCode, merchantPassword, cardWithCvv, currencyAmount, None, Some(deal)) returns someOrderCode

      worldpayGateway.authorize(someValidMerchant, cardWithCvv, currencyAmount, None, Some(deal)) must
        be_===(Success(authorizationKey))
    }

    "gracefully return a reject error upon a business-related error" in new Ctx {
      // worldpay service returns REFUSED response when the card holder name is REFUSED
      val rejectedCreditCard = creditCard.copy(
        additionalFields = Some(CreditCardOptionalFields.withFields(
          holderName = Some("REFUSED"))))

      driver.anAuthorizationRequest(
        merchantCode,
        merchantPassword,
        rejectedCreditCard,
        currencyAmount,
        None,
        Some(deal)) refuses("Some error num", "me$$age")

      worldpayGateway.authorize(someValidMerchant, rejectedCreditCard, currencyAmount, None, Some(deal)) must
        beAFailedTry.like {
          case e: PaymentRejectedException => e.message must beEqualTo("Error code: Some error num, Error message: me$$age")
        }
    }

    "gracefully return a payment error upon a technical error" in new Ctx {
      // worldpay service returns ERROR response when the card holder name is ERROR
      val errorCreditCard = creditCard.copy(
        additionalFields = Some(CreditCardOptionalFields.withFields(
          holderName = Some("ERROR"))))

      driver.anAuthorizationRequest(
        merchantCode,
        merchantPassword,
        errorCreditCard,
        currencyAmount,
        None,
        Some(deal)) errors("Some error num", "me$$age")

      worldpayGateway.authorize(someValidMerchant, errorCreditCard, currencyAmount, None, Some(deal)) must
        beAFailedTry.like {
          case e: PaymentErrorException => e.message must beEqualTo("Error code: Some error num, Error message: me$$age")
        }
    }

    "gracefully return an error upon a gateway exception" in new Ctx {
      val errorMessage = "This request requires HTTP authentication."
      val statusCode = StatusCodes.Unauthorized

      driver.anAuthorizationRequest(
        merchantCode,
        invalidMerchantPassword,
        creditCard,
        currencyAmount,
        None,
        Some(deal)) failsWithStatusAndMessage(statusCode, errorMessage)

      worldpayGateway.authorize(someInvalidMerchant, creditCard, currencyAmount, None, Some(deal)) must
        beAFailedTry.like {
          case e: PaymentErrorException => e.message must (contain(s"Worldpay server returned ${statusCode.intValue}:") and contain(errorMessage))
        }
    }
  }


  "capture request" should {
    val currencyAmount = CurrencyAmount("USD", 50)
    val authorizationKey = anAuthorizationKeyFor(currencyAmount.currency)

    "successfully yield an authorization key upon a valid request" in new Ctx {
      driver.aCaptureRequest(merchantCode, merchantPassword, someOrderCode, currencyAmount) returns someOrderCode

      worldpayGateway.capture(someValidMerchant, authorizationKey, currencyAmount.amount) must
        be_===(Success(authorizationKey))
    }

    "successfully yield an authorization key upon a valid request with different currency" in new Ctx {
      val differentCurrency = currencyAmount.copy(currency = "ISK")
      val differentAuthorizationKey = anAuthorizationKeyFor(differentCurrency.currency)

      driver.aCaptureRequest(
        merchantCode, merchantPassword, someOrderCode, differentCurrency, 0) returns someOrderCode

      worldpayGateway.capture(someValidMerchant, differentAuthorizationKey, differentCurrency.amount) must
        be_===(Success(differentAuthorizationKey))
    }

    "gracefully return a payment error a technical error" in new Ctx {
      // generate some invalid data
      val invalidCurrencyAmount = currencyAmount.copy(currency = "WIX_DOLLAR")
      val invalidAuthorizationKey = anAuthorizationKeyFor(invalidCurrencyAmount.currency)

      driver.aCaptureRequest(
        merchantCode,
        merchantPassword,
        someOrderCode,
        invalidCurrencyAmount) errors("Some error code", "me$$age")

      worldpayGateway.capture(someValidMerchant, invalidAuthorizationKey, invalidCurrencyAmount.amount) must
        beAFailedTry.like {
          case e: PaymentErrorException => e.message must beEqualTo("Error code: Some error code, Error message: me$$age")
        }
    }

    "gracefully return an error upon a gateway exception" in new Ctx {
      val errorMessage = "This request requires HTTP authentication."
      val statusCode = StatusCodes.Unauthorized

      driver.aCaptureRequest(
        merchantCode,
        invalidMerchantPassword,
        someOrderCode,
        currencyAmount) failsWithStatusAndMessage(statusCode, errorMessage)

      worldpayGateway.capture(someInvalidMerchant, authorizationKey, currencyAmount.amount) must
        beAFailedTry.like {
          case e: PaymentErrorException => e.message must (contain(s"Worldpay server returned ${statusCode.intValue}:") and contain(errorMessage))
        }
    }

    "gracefully return a paymentErrorException upon an invalid request" in new Ctx {
      worldpayGateway.capture(someValidMerchant, "Some invalid json code", currencyAmount.amount) must
        beAFailedTry(
          check = beAnInstanceOf[PaymentErrorException]
        )
    }
  }


  "sale request" should {
    val creditCard = CreditCard(
      number = "4580458045804580",
      expiration = YearMonth(2020, 12),
      additionalFields = Some(CreditCardOptionalFields.withFields(
        holderName = Some("kukibuki"))))
    val currencyAmount = CurrencyAmount("USD", 100)
    val deal = Deal("123", Some("title"), Some("desc"))
    val authorizationKey = anAuthorizationKeyFor(currencyAmount.currency)

    "successfully yield an authorization key upon a valid request" in new Ctx {
      driver.anAuthorizationRequest(
        merchantCode, merchantPassword, creditCard, currencyAmount, None, Some(deal)) returns someOrderCode
      driver.aCaptureRequest(merchantCode, merchantPassword, someOrderCode, currencyAmount) returns someOrderCode

      worldpayGateway.sale(someValidMerchant, creditCard, currencyAmount, None, Some(deal)) must
        be_===(Success(authorizationKey))

      driver.requests must
        containsASaleTransactionFor(
          merchantKey = merchantCode,
          orderCode = someOrderCode,
          currency =  "USD",
          amount = 10000,
          cardNode = "VISA-SSL",
          creditCard = creditCard)
    }
  }

  "voidAuthorization request" should {
    val authorizationKey = anAuthorizationKeyFor("USD")

    "successfully yield an authorization key upon a valid request" in new Ctx {
      driver.aVoidAuthorizationRequest(merchantCode, merchantPassword, someOrderCode) returns someOrderCode

      worldpayGateway.voidAuthorization(someValidMerchant, authorizationKey) must be_===(Success(authorizationKey))
    }

    "gracefully return an error upon a gateway exception" in new Ctx {
      val errorMessage = "This request requires HTTP authentication."
      val statusCode = StatusCodes.Unauthorized

      driver.aVoidAuthorizationRequest(
        merchantCode, invalidMerchantPassword, someOrderCode) failsWithStatusAndMessage(statusCode, errorMessage)

      worldpayGateway.voidAuthorization(someInvalidMerchant, authorizationKey) must
        beAFailedTry.like {
          case e: PaymentErrorException => e.message must (contain(s"Worldpay server returned ${statusCode.intValue}:") and contain(errorMessage))
        }
    }

    "gracefully return a paymentErrorException upon an invalid request" in new Ctx {
      worldpayGateway.voidAuthorization(someValidMerchant, "Some invalid json code") must
        beAFailedTry(
          check = beAnInstanceOf[PaymentErrorException]
        )
    }
  }


  step {
    driver.stop()
  }
}
