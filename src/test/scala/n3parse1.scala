package org.w3.swap.test

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

  val expectedNum: List[IO] = List(
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
       List[Any](dec("1324.2324"), 234.0e+34, dec("-1.2"), dec("+1.2")))
  )

  val expectedOther: List[IO] = List(
    IO("<abc> foo:bar ?x _:y",
       List[Any]("abc", QName("foo", "bar"), "x", "y") ),
    IO("<abc#def> foo:bar 123",
       List[Any]("abc#def", QName("foo", "bar"), 123) )
    )

  class N3TokenList extends N3Lex {
    /* darn... to implement the longest-matching rule, this is order-sensitive*/
    def tokens: Parser[List[Any]] = rep(
      double | decimal | integer
      | uriref | qname
      | evar | uvar
    )
  }

  def lexTest(io: IO): Boolean = {
    val lexer = new N3TokenList()
    val result = lexer.parseAll(lexer.tokens, io.in)

    result match {
      case lexer.Success(tokens, _ ) => tokens == io.out
      case lexer.Failure(_, _) | lexer.Error(_, _) => false
    }
  }

  /* scala note:
   * Gen.oneOf() takes repeated parameters; _* lets us call
   * it on a list. */
  property ("numerals tokenize correctly") =
    Prop.forAll(Gen.oneOf(expectedNum.map(Gen.value): _*)){
      io => lexTest(io)
    }


  property ("other tokens tokenize correctly") =
    Prop.forAll(Gen.oneOf(expectedOther.map(Gen.value): _*)){
      io => lexTest(io)
    }
}

object n3parsing extends Properties("N3 Parsing") {
  import org.w3.swap.logicalsyntax.{Formula, And, NotNil, Apply, Literal}
  import org.w3.swap.rdf2004.URI
  import org.w3.swap.N3Parser

  case class IO(in: String, out: Formula)

  val expected = List(
    IO("", And(List())),
    IO("<#pat> <#knows> <#joe>.",
       And(List(NotNil(Apply('holds, List(URI("data:#pat"),
					  URI("data:#knows"),
					  URI("data:#joe") )))))
       ),
    IO("<#pat> <#age> 23.",
       And(List(NotNil(Apply('holds, List(URI("data:#pat"),
					  URI("data:#age"),
					  Literal(23) )))))
       ),
    IO("<#pat> <#name> \"Pat\".",
       And(List(NotNil(Apply('holds, List(URI("data:#pat"),
					  URI("data:#name"),
					  Literal("Pat") )))))
       ),
    IO("<#pat> <#age> 23. <#pat> <#name> \"Pat\".",
       And(List(NotNil(Apply('holds, List(URI("data:#pat"),
					  URI("data:#age"),
					  Literal(23) ))),
		NotNil(Apply('holds, List(URI("data:#pat"),
					  URI("data:#name"),
					  Literal("Pat") ))) )) )
  )

  def parseTest(io: IO): Boolean = {
    val parser = new N3Parser("data:")
    val result = parser.parseAll(parser.document, io.in)

    result match {
      case parser.Success(f, _ ) => f == io.out
      case parser.Failure(_, _) | parser.Error(_, _) => false
    }
  }

  property ("empty formula parses") =
    Prop.forAll(Gen.oneOf(expected.map(Gen.value): _*)){ io =>
      parseTest(io)
    }
}
