package org.w3.swap.sexp

sealed abstract class SExp {
  def print(): String = {
    val b = new StringBuilder()
    printTo(b)
    b.toString()
  }

  def printTo(builder: StringBuilder)
}

case class Cons(head: SExp, tail: SExp) extends SExp {
  import SExp.printTail

  override def printTo(builder: StringBuilder) {

    head match {
      case Atom('quote) => {
	tail match {
	  case Cons(v, _) => {
	    builder.append("'")
	    v.printTo(builder)
	  }
	  case _ => throw new Exception("bad 'quote tail:" + this.toString())
	}
      }

      case _ => {
	builder.append("(")
	head.printTo(builder)
	printTail(tail, builder)
      }
    }
  }

  /* TODO: pretty-printing */
}

case class Atom(val x: Any) extends SExp {
  override def printTo(builder: StringBuilder) {
    x match {
      /* lisp syntax, not scala syntax.
       * Well... close, anyway... lowercase symbols
       * and such should print as |foo|. */
      case s: Symbol => builder.append(s.name)
      case str: String => {
	builder.append("\"")
	builder.append(str) /* @@TODO: escaping */
	builder.append("\"")
      }
      case i: Int => builder.append(i.toString())
      /* TODO: print decimals as (/ num denom) */

      case _ => throw new Exception("huh? what's that?" + x.toString())
    }
  }
}

object SExp {
  val NIL = Atom('NIL)

  def fromSeq(items: Seq[Any]): SExp = {
    if (items.isEmpty) NIL
    else {
      val hexp: SExp = items.head match {
	case e: SExp => e
	case s: Seq[_] => fromSeq(s)
	case _ => Atom(items.head)
      }
      Cons(hexp, fromSeq(items.tail))
    }
  }

  implicit def fromSymbol(s: Symbol): SExp = Atom(s)

  /* scalaQ: private? */
  def printTail(e: SExp, builder: StringBuilder) {
    e match {
      case Cons(h, t) => {
	builder.append(" ")
	h.printTo(builder)
	printTail(t, builder)
      }
      case NIL => builder.append(")") // TODO: linebreaks in sexp printing
      case _ => {
	builder.append(". ")
	e.printTo(builder)
      }
    }
  }
}
