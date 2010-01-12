/* tests for N3 parsing
 * */

import org.scalacheck._
import Prop._
import Arbitrary.arbitrary


object numberLex extends Properties("N3 tokenization") {
  import org.w3.swap.N3Lex
  import org.w3.swap.QName

  case class IO(in: String, out: List[Any])

  def dec(s: String) = new java.math.BigDecimal(s)

  val expected: List[IO] = List(
    // TODO: what about really big integers?

    /* scala note: List(...) alone allows ints to be converted
     * to doubles, so we have to use List[Any](...)
     * Thanks to paulp_ in #scala on FreeNode for the clue.*/

    IO("1 234e0 1.2",
       List[Any](1, 234.0, dec("1.2"))),
    IO("  982739\t234e0 132432432432432432.2324",
       List[Any](982739, 234.0, dec("132432432432432432.2324")) ),

    IO("-982739 +93287 234.234e3 -234e3",
       List[Any](-982739, +93287, 234.234e3, -234e3) ),
    IO("234e0 +234e34 +234e-34 234e-34 234e-34",
       List[Any](234e0, +234e34, +234e-34, 234e-34, 234e-34) ),
    IO("1324.2324 234e+34 -1.2 +1.2",
       List[Any](dec("1324.2324"), 234.0e+34, dec("-1.2"), dec("+1.2"))),
    IO("<abc> foo:bar ?x _:y",
       List[Any]("data:abc", QName("foo", "bar"), "x", "y") )
  )

  /* Gen.oneOf() takes repeated parameters; _* lets us call
   * it on a list. */
  val genExpected = Gen.oneOf(expected.map(Gen.value): _*)

  class N3TokenList extends N3Lex("data:") {
    /* darn... to implement the longest-matching rule, this is order-sensitive*/
    def tokens: Parser[List[Any]] = rep(
      double | decimal | integer
      | uriref | qname
      | evar | uvar
    )
  }

  property ("numerals tokenize to the right classes in many cases") =
    Prop.forAll(genExpected){
      io => {
	val lexer = new N3TokenList()
	val result = lexer.parseAll(lexer.tokens, io.in)

	result match {
	  case lexer.Success(tokens, _ ) => tokens == io.out
	  case lexer.Failure(_, _) | lexer.Error(_, _) => false
	}
      }
    }
}

