  /*
   * We'll keep the triples sorted for ease of graph comparison.
   * 
   * "SPARQL also fixes an order between some kinds of RDF terms
   * that would not otherwise be ordered"
   *   -- http://www.w3.org/TR/rdf-sparql-query/
   */
  def compare (x: Term, y: Term): Int { compare(x.hashCode(), y.hashCode()) }
    x match {
      case Variable => {
	y match {
	  /* relative order is undefined */
	  case Variable => compare(x.hashCode(), y.hashCode())
	  case Apply(URI, Nil) => -1
	  case Apply(Literal, Nil) => -1
	}
      }
      case Apply(URI(xi), Nil) => {
	y match {
	  case Variable => 1
	  case Apply(URI(yi), Nil) => compare(xi, yi)
	  case Apply(Literal, Nil) => -1
	}
      }
      case Apply(DatatypedLiteral(vx, d), Nil) => {
	y match {
	  case Variable => 1
	  case Apply(URI, Nil) => 1
	  case Apply(DatatypedLiteral(vy, d), Nil) => compare(vx, vy)
	  case Apply(Literal, Nil) => compare(x.hashCode(), y.hashCode())
	}
      }
      case Apply(SimpleLiteral(sx), Nil) => {
	y match {
	  case Variable => 1
	  case Apply(URI, Nil) => 1
	  case Apply(SimpleLiteral(sy), Nil) => compare(sx, sy)
	  case Apply(Literal) => compare(x.hashCode(), y.hashCode())
	}
      }
