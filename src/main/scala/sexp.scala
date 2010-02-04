package org.w3.swap.sexp

import java.io.Writer

sealed abstract class SExp {
  def print(): String = {
    val b = new java.io.StringWriter()
    writeTo(b)
    b.toString()
  }

  def writeTo(w: Writer)
}

case class Cons(head: SExp, tail: SExp) extends SExp {
  import SExp.writeTail

  override def writeTo(w: Writer) {

    head match {
      case Atom('quote) => {
	tail match {
	  case Cons(v, _) => {
	    w.append("'")
	    v.writeTo(w)
	  }
	  case _ => throw new Exception("bad 'quote tail:" + this.toString())
	}
      }

      case _ => {
	w.append("(")
	head.writeTo(w)
	writeTail(tail, w)
      }
    }
  }

  /* TODO: pretty-printing */
}

case class Atom(val x: Any) extends SExp {
  override def writeTo(w: Writer) {
    x match {
      /* lisp syntax, not scala syntax.
       * Well... close, anyway... lowercase symbols
       * and such should print as |foo|. */
      case s: Symbol => w.append(s.name)
      case str: String => {
	w.append("\"")
	w.append(str) /* @@TODO: escaping */
	w.append("\"")
      }
      case i: Int => w.append(i.toString())
      /* TODO: print decimals as (/ num denom) */

      case _ => throw new Exception("@@huh? what's that?" + x.toString())
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
  def writeTail(e: SExp, w: Writer) {
    e match {
      case Cons(h, t) => {
	w.append(" ")
	h.writeTo(w)
	writeTail(t, w)
      }
      case NIL => w.append(")") // TODO: linebreaks in sexp printing
      case _ => {
	w.append(". ")
	e.writeTo(w)
      }
    }
  }
}
