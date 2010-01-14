package org.w3.swap.test

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import org.w3.swap

class URIPathTest extends Spec with ShouldMatchers {
  import swap.uri.Util.combine

  describe ("Combining base URI with URI reference") {

    it ("should handle ..") {
      combine("http://example/x/y/z", "../abc") should equal (
	"http://example/x/abc")
    }

    it ("should handle the empty ref") {
      combine("http://example/x/y/z", "") should equal (
	"http://example/x/y/z")
    }

    it ("should handle data: as a base URI") {
      combine("data:", "bob") should equal (
	"data:bob" )
    }
  }
}

