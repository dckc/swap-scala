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
  import org.w3.swap.logicalsyntax.{Formula, Exists, And, NotNil,
				    Apply, Literal}
  import org.w3.swap.rdf2004.{URI, BlankNode}
  import org.w3.swap.N3Parser
  import org.w3.swap.N3AbstractSyntax.atom

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
    IO("22 <#lessThan> 23.",
       And(List(NotNil(Apply('holds, List(Literal(22),
					  URI("data:#lessThan"),
					  Literal(23) )))))
       ),
    IO("<#pat> has <#age> 23.",
       And(List(NotNil(Apply('holds, List(URI("data:#pat"),
					  URI("data:#age"),
					  Literal(23) )))))
       ),
    IO("23 is <#age> of <#pat>.",
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
					  Literal("Pat") ))) )) ),
    IO("<#pat> <#child> [ <#age> 4 ].",
       {
	 val e1 = BlankNode("e", Some(0))
	 def i(s: String): URI = URI("data:#" + s)
	 Exists(List(e1),
		And(List(atom(e1, i("age"), Literal(4)),
			 atom(i("pat"), i("child"), e1) )) )
       })

    /* for later...
    IO("""@prefix : <#>.
       @prefix owl: <data:owl#>.
       @prefix rdfs: <data:rdfs#>.
       { ?pizza :toppings ?L.
       ?C owl:oneOf ?L.
       ?C rdfs:subClassOf [ owl:complementOf :MeatToppings] }
       => { ?pizza a :VegetarianPizza }""",
       Forall(List(Var("pizza"), Var("L"), Var("C")),
	      Implies(
		Exists(Exists(List(BlankNode("", Some((6, 20)))),
			      And(atom(Var("pizza"),
				       URI("data:#toppings"),
				       Var("L") ),
				  atom(Var("C")
				       URI("data:owl#oneOf"),
				       Var("L") ),
				  ...
     */
       
  )

  def parseTest(txt: String): Option[Formula] = {
    val parser = new N3Parser("data:")
    val result = parser.parseAll(parser.document, txt)

    result match {
      case parser.Success(f, _ ) => Some(f)
      case parser.Failure(x, y) => {
	println("@@failure")
	println(x)
	println(y.pos.longString)
	None
      }
      case parser.Error(x, y) => {
	println("@@error")
	println(x)
	println(y)
	None
      }
    }
  }

  property ("test inputs give expected formulas") =
    Prop.forAll(Gen.oneOf(expected.map(Gen.value): _*)){ io =>
      parseTest(io.in) match {
	case Some(f) => ("wrong parse result: " + f.toString) |: f == io.out
	case None => "parser Failure or Error" |: false
      }
    }
}
