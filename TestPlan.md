The main goal is confidence in an N3Logic proof checker.
The plan to get there (with status) is:

  * standard RDF (not RDFS) entailment tests. DONE.
    * RDF/XML parser (working well enough for this task)
    * n-triples parser (working)
    * RDF proof generator, or
    * RDF proof checker
> > > Currently, we have an RDF entailment method, though some questions
> > > about variable handling remain.
  * standard RDF positive/negative syntax tests (optional)
  * Dave's turtle syntax tests (optional), which needs
    * turtle parser or perhaps just
      * N3 parser (working; feature complete for turtle high level structures,
> > > > but not low-level details such as string escaping)
    * RDF entailment method
  * standard [RIF BLD entailment tests](http://www.w3.org/TR/rif-test/),

> > which needs
    * RIF BLD XML reader
    * N3Rules proof generator, or
      * N3 proof reader
    * N3Rules proof checker
  * standard RDFS tests, which needs
    * RDF/XML parser
    * n-triples parser
    * RDFS rules in N3Rules
    * N3Rules proof generator, checker
  * [N3 syntax tests](http://www.w3.org/2000/10/swap/test/n3parser.tests), (optional) which needs
    * N3 parser (working; not feature-complete)
  * some sort of N3Logic proof testing, which needs
    * N3Logic proof generator and/or N3 proof reader (and use cwm)
    * N3Logic proof checker