package org.w3.swap.sexp

import java.io.Writer

/*
 * scala docs say:
 *   based on Lindig's strict version of Wadler's adaptation of
 *   Hughes' pretty-printer.
 * found it:
 *   Strictly Pretty (2000)
 *   by Christian Lindig ,  Gartner Datensysteme
 *   http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.2200
 *   formerly at:
 *   http://www.gaertner.de/~lindig/papers/download/strictly-pretty.ps.gz
 */
import scala.text.{Document, DocText, DocGroup, DocCons, DocNest}

sealed abstract class SExp {
  def print(): String = {
    val b = new java.io.StringWriter()
    writeTo(b)
    b.toString()
  }

  def pretty(): String = {
    val w = new java.io.StringWriter()
    doc.format(72, w)
    w.toString()
  }

  override def toString() = pretty()

  def doc: Document

  def writeTo(w: Writer)
}

case class Cons(head: SExp, tail: SExp) extends SExp {
  import SExp.{writeTail, docTail}

  override def doc = {
    head match {
      case Atom(x) if x == 'QUOTE => {
	tail match {
	  case Cons(v, rest) if rest == SExp.NIL => {
	    DocGroup("'" :: v.doc)
	  }
	  case _ => throw new Exception("bad 'quote tail:" + this.toString())
	}
      }

      case _ => {
	DocGroup("(" :: DocNest(2, head.doc :/: docTail(tail)))
      }
    }
  }


  override def writeTo(w: Writer) {

    head match {
      case Atom(x) if x == 'QUOTE => {
	tail match {
	  case Cons(v, rest) if rest == SExp.NIL => {
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

  override def toString(): String = {
    x match {
      case s: Symbol if s == 'NIL => "()"
      /* lisp syntax, not scala syntax.
       * Well... close, anyway... lowercase symbols
       * and such should print as |foo|. */
      case s: Symbol => s.name
      case str: String => {
	("\"" +
	 str + /* @@TODO: escaping */
	 "\"")
      }

      /* TODO: something reasonable with XML */
      /* TODO: print decimals as (/ num denom) */
      case _ => x.toString()
    }
  }

  override def doc = DocText(this.toString())

  override def writeTo(w: Writer) = w.append(this.toString())
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

  def docTail(e: SExp): Document = {
    e match {
      case Cons(h, t) => h.doc :/: docTail(t)
      case NIL => DocText(")")
      case _ => DocText(".") :/: e.doc
    }
  }

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
