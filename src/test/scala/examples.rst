The main package is swap::

  scala> import org.w3.swap
  import org.w3.swap

Parsing N-Triples
-----------------

Instantiate the parser class::

  scala> import swap.ntriples.NTriplesParser
  import swap.ntriples.NTriplesParser

  scala> val p = new NTriplesParser()
  p: org.w3.swap.ntriples.NTriplesParser = org.w3.swap.ntriples.NTriplesParser@16e16c5

Then consider this document; note N-Triples is picky about where
linebreaks are allowed::

  scala> val doc2 = "<data:bob> <data:age> \"23\".\n"
  doc2: java.lang.String = 
  <data:bob> <data:age> "23".

And finally, parse it::

  scala> p.parseAll(p.ntripleDoc, doc2)
  res9: p.ParseResult[org.w3.swap.logic.Formula] = [2.1] parsed: (and (holds (data:bob) (data:age) "23"))

