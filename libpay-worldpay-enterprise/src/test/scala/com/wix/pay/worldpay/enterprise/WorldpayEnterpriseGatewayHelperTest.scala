package com.wix.pay.worldpay.enterprise

import com.wix.pay.PaymentErrorException
import com.wix.pay.creditcard.{CreditCard, CreditCardOptionalFields, YearMonth}
import com.wix.pay.model.{CurrencyAmount, Deal}
import org.specs2.mutable.SpecWithJUnit


class WorldpayEnterpriseGatewayHelperTest extends SpecWithJUnit with WorldpayEnterpriseMatchers {

  "create authorization request" should {
    val merchantKey = "bibibuzi"
    val creditCard = CreditCard(
      number = "4580458045804580",
      expiration = YearMonth(2020, 12),
      additionalFields = Some(CreditCardOptionalFields.withFields(holderName = Some("obama"))))
    val currencyAmount = CurrencyAmount("USD", 50.00)
    val deal = Deal(
      id = "someId"
    )

    "return a valid authorization request for 50 USD" in {
      WorldpayEnterpriseGatewayHelper.createAuthorizationRequest(merchantKey, creditCard, currencyAmount, deal) must
        beAValidAuthorizationRequest(merchantKey = ===(merchantKey),
                                     paymentDetails = beAValidPaymentDetailsFor("VISA-SSL", creditCard),
                                     currencyAmount = beAValidCurrencyAmount("USD", 5000, 2))
    }

    "return a valid authorization request for 5000 ISK" in {
      val differentCurrencyAmount = CurrencyAmount("ISK", 5000)

      WorldpayEnterpriseGatewayHelper.createAuthorizationRequest(merchantKey, creditCard, differentCurrencyAmount, deal) must
        beAValidAuthorizationRequest(merchantKey = ===(merchantKey),
                                     paymentDetails = beAValidPaymentDetailsFor("VISA-SSL", creditCard),
                                     currencyAmount = beAValidCurrencyAmount("ISK", 5000, 0))
    }

    "return a valid authorization request for a credit card with cvc" in {
      // Worldpay's test card number for VISA
      val card = creditCard.copy(
        number = "4580458045804580",
        additionalFields = Some(CreditCardOptionalFields(
          csc = Some("555"),
          publicFields = creditCard.additionalFields flatMap (_.publicFields))))

      WorldpayEnterpriseGatewayHelper.createAuthorizationRequest(merchantKey, card, currencyAmount, deal) must
        beAValidAuthorizationRequest(paymentDetails = beAValidPaymentDetailsFor("VISA-SSL", card))
    }


    /* Test supported credit cards */
    "return a valid authorization request for Visa" in {
      // Worldpay's test card number for VISA
      val card = creditCard.copy(number = "4580458045804580")

      WorldpayEnterpriseGatewayHelper.createAuthorizationRequest(merchantKey, card, currencyAmount, deal) must
        beAValidAuthorizationRequest(paymentDetails = beAValidPaymentDetailsFor("VISA-SSL", card))
    }

    "return a valid authorization request for Mastercard" in {
      // Worldpay's test card number for MasterCard
      val card = creditCard.copy(number = "5555555555554444")

      WorldpayEnterpriseGatewayHelper.createAuthorizationRequest(merchantKey, card, currencyAmount, deal) must
        beAValidAuthorizationRequest(paymentDetails = beAValidPaymentDetailsFor("ECMC-SSL", card))
    }

    "return a valid authorization request for American Express" in {
      // Worldpay's test card number for American Express
      val card = creditCard.copy(number = "343434343434343")

      WorldpayEnterpriseGatewayHelper.createAuthorizationRequest(merchantKey, card, currencyAmount, deal) must
        beAValidAuthorizationRequest(paymentDetails = beAValidPaymentDetailsFor("AMEX-SSL", card))
    }

    "return a valid authorization request for Diners card" in {
      // Worldpay's test card number for Diners
      val card = creditCard.copy(number = "36700102000000")

      WorldpayEnterpriseGatewayHelper.createAuthorizationRequest(merchantKey, card, currencyAmount, deal) must
        beAValidAuthorizationRequest(paymentDetails = beAValidPaymentDetailsFor("DINERS-SSL", card))
    }

    "return a valid authorization request for Discover card" in {
      // Worldpay's test card number for Discover Card
      val card = creditCard.copy(number = "6011000400000000")

      WorldpayEnterpriseGatewayHelper.createAuthorizationRequest(merchantKey, card, currencyAmount, deal) must
        beAValidAuthorizationRequest(paymentDetails = beAValidPaymentDetailsFor("DISCOVER-SSL", card))
    }

    "return a valid authorization request for Maestro card" in {
      // Worldpay's test card number for Maestro
      val card = creditCard.copy(number = "6759649826438453")

      WorldpayEnterpriseGatewayHelper.createAuthorizationRequest(merchantKey, card, currencyAmount, deal) must
        beAValidAuthorizationRequest(paymentDetails = beAValidPaymentDetailsFor("MAESTRO-SSL", card))
    }

    "throw an unsupported credit card exception for an invalid credit card" in {
      val card = creditCard.copy(number = "12345678903")

      WorldpayEnterpriseGatewayHelper.createAuthorizationRequest(merchantKey, card, currencyAmount, deal) must
        throwA(PaymentErrorException("Unknown credit card network"))
    }
  }

  "toWorldpayAmount" should {
    "return 1000 for 10 USD" in {
      WorldpayEnterpriseGatewayHelper.toWorldpayAmount(CurrencyAmount("USD", 10)) must be_==(1000)
    }

    "return 5050 for 5050 JPY" in {
      WorldpayEnterpriseGatewayHelper.toWorldpayAmount(CurrencyAmount("JPY", 5050)) must be_==(5050)
    }
  }

  "create capture request" should {
    val merchantKey = "bibibuzi"
    val orderCode = "123"

    "return a valid capture request for 50 USD" in {
      val currencyAmount = CurrencyAmount("USD", 50.00)

      WorldpayEnterpriseGatewayHelper.createCaptureRequest(merchantKey, orderCode, currencyAmount) must
        beAValidModificationRequest(merchantKey = merchantKey,
                                    orderCode = orderCode,
                                    modificationType = "capture",
                                    modification = beAValidCurrencyAmount("USD", 5000, 2))
    }

    "return a valid capture request for 5000 ISK" in {
      val currencyAmount = CurrencyAmount("ISK", 5000)

      WorldpayEnterpriseGatewayHelper.createCaptureRequest(merchantKey, orderCode, currencyAmount) must
        beAValidModificationRequest(merchantKey = merchantKey,
          orderCode = orderCode,
          modificationType = "capture",
          modification = beAValidCurrencyAmount("ISK", 5000, 0))
    }
  }

  "create refund request" should {
    val merchantKey = "bibibuzi"
    val orderCode = "123"

    "return a valid refund request for 50 USD" in {
      val currencyAmount = CurrencyAmount("USD", 50.00)

      WorldpayEnterpriseGatewayHelper.createRefundRequest(merchantKey, orderCode, currencyAmount) must
        beAValidModificationRequest(merchantKey = merchantKey,
          orderCode = orderCode,
          modificationType = "refund",
          modification = beAValidCurrencyAmount("USD", 5000, 2))
    }

    "return a valid refund request for 5000 JPY" in {
      val currencyAmount = CurrencyAmount("JPY", 5000)

      WorldpayEnterpriseGatewayHelper.createRefundRequest(merchantKey, orderCode, currencyAmount) must
        beAValidModificationRequest(merchantKey = merchantKey,
          orderCode = orderCode,
          modificationType = "refund",
          modification = beAValidCurrencyAmount("JPY", 5000, 0))
    }
  }

  "create void authorization request" should {
    val merchantKey = "bibibuzi"
    val orderCode = "123"

    "return a valid voidAuthorization request" in {
      WorldpayEnterpriseGatewayHelper.createVoidAuthorizationRequest(merchantKey, orderCode) must
        beAValidModificationRequest(merchantKey = merchantKey,
          orderCode = orderCode,
          modificationType = "cancel")
    }
  }

  "create void request" should {
    val merchantKey = "bibibuzi"
    val orderCode = "123"

    "return a valid void request" in {
      WorldpayEnterpriseGatewayHelper.createVoidRequest(merchantKey, orderCode) must
        beAValidModificationRequest(merchantKey = merchantKey,
          orderCode = orderCode,
          modificationType = "cancelOrRefund")
    }
  }
}
