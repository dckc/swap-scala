package org.w3.swap.test

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import org.w3.swap

class SExpTest extends Spec with ShouldMatchers {
  import swap.sexp.Cons
  import swap.sexp.SExp.{NIL}

  describe("SExp"){
    it("should convert a simple s-exp to a string"){
      (Cons('abc, Cons('def, NIL)).print()) should equal (
	"(abc def)")
    }
  }
}
