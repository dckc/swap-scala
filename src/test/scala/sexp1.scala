package org.w3.swap

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class SExpTest extends Spec with ShouldMatchers {
  import SExp.{nil}

  describe("SExp"){
    it("should convert a simple s-exp to a string"){
      (Cons("abc", Cons("def", nil)).toString()) should equal (
	"(abc def)")
    }
  }
}
