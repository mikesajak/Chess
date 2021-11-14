package com.mikesajak.chess

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

trait UnitTestSpec extends AnyFlatSpec with should.Matchers with OptionValues
    with Inside with Inspectors
