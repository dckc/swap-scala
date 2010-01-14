package org.w3.swap

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class SExpTest extends Spec with ShouldMatchers {
  import sexp.Cons
  import sexp.SExp.{NIL}

  describe("SExp"){
    it("should convert a simple s-exp to a string"){
      (Cons('abc, Cons('def, NIL)).print()) should equal (
	"(abc def)")
    }
  }
}
