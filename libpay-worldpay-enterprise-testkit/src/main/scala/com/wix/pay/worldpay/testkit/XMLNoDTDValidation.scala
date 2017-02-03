package com.wix.pay.worldpay.testkit

import javax.xml.parsers.SAXParser

import scala.xml.Elem
import scala.xml.factory.XMLLoader

/**
 * Workaround for loading and validating the given XML string against its schema file (dtd) if specified.
 * This XML loader ignore the dtd specification while parsing an XML file, which makes the loading fast.
 *
 * * @author <a href="mailto:lidanh@wix.com">Lidan Hifi</a>
 */
object XMLNoDTDValidation extends XMLLoader[Elem] {
  override def parser: SAXParser = {
    val f = javax.xml.parsers.SAXParserFactory.newInstance()
    f.setValidating(false)
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
    f.newSAXParser()
  }
}

