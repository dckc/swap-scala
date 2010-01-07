package org.w3.swap

sealed case class SExp()
case class Cons(head: SExp, tail: SExp) extends SExp {
  override def toString(): String = {
    def tailString(e: SExp): String = {
      e match {
	case Symbol("nil") => ")"
	case Symbol(s) => ". " + s
	case Cons(h, t) => " " + h.toString() + tailString(t)
      }
    }
    "(" + head.toString() + tailString(tail)
  }
}

case class Symbol(name: String) extends SExp {
  override def toString(): String = name
}

object SExp {
  val nil = Symbol("nil")
  implicit def makeSymbol(n: String) = Symbol(n)
}

