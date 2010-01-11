package org.w3.swap

sealed abstract class SExp {
  def print(): String
}


case class Cons(head: SExp, tail: SExp) extends SExp {
  import SExp.tailString

  override def print() = {
    head match {
      case Atom('quote) => {
	tail match {
	  case Cons(v, _) => "'" + v.print()
	  case _ => throw new Exception("bad 'quote tail:" + tail.toString())
	}
      }
      case _ => "(" + head.print() + tailString(tail)
    }
  }

  /* TODO: streaming ... e.g. lazy Stream[String] */
  /* TODO: pretty-printing */
}

case class Atom(x: Any) extends SExp {
  override def print() = {
    x match {
      /* lisp syntax, not scala syntax.
       * Well... close, anyway... lowercase symbols
       * and such should print as |foo|. */
      case s: Symbol => s.name
      case str: String => "\"" + str + "\"" /* TODO: escaping */
      case i: Int => i.toString()
      /* TODO: print decimals as (/ num denom) */

      case _ => throw new Exception("huh? what's that?" + x.toString())
    }
  }
}

object SExp {
  val NIL = Atom('NIL)

  implicit def fromList(items: List[Any]): SExp = {
    items match {
      case Nil => NIL
      case hd :: tl => {
	val hexp = hd match {
	  case l: List[_] => fromList(l)
	  case e: SExp => e
	  case _ => Atom(hd)
	}
	Cons(hexp, fromList(tl))
      }
    }
  }

  implicit def fromSymbol(s: Symbol): SExp = Atom(s)

  /* scalaQ: private?
   * scalaQ: doesn't depend on head/tail; move out of this class somehow? */
  def tailString(e: SExp): String = {
    e match {
      case Cons(h, t) => " " + h.print() + tailString(t)
      case NIL => ")"
      case _ => ". " + e.print()
    }
  }

}
