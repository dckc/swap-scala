package org.w3.swap.logic1n3

import java.math.BigDecimal

import org.w3.swap
import swap.logic1
import swap.logic1.{Term, FunctionTerm, Variable}
import Term.Subst
import swap.logic1cl.{Conjunction, Implication,
		      Implies, Exists, And, Or, Atomic}
import swap.rdf.{RDFGraphParts}
import swap.rdflogic.{Name, Ground}
import swap.n3.{N3Syntax, N3TermBuilder}


case class Var(s: Scope, i: String, n: Int, universal: Boolean) extends Variable
case class BoolT(v: Boolean) extends Ground
case class IntT(v: Int) extends Ground
case class StringT(v: String) extends Ground
case class DoubleT(v: Double) extends Ground
case class DecimalT(v: BigDecimal) extends Ground

case class App(sym: Symbol, params: List[Term]) extends FunctionTerm  {
  override def fun = sym
  override def args = params
  override def subst(s: Subst) = App(sym, params.map(_.subst(s)))
}


class Scope {
  import scala.collection.mutable
  protected val varstack = new mutable.Stack[Var]

  val theory = new mutable.Stack[Implication]

  def byName(name: String, u: Boolean): Var = {
    varstack.find { v => v.i == name } match {
      case Some(v) => v
      case None => {
	val v = Var(this, name, varstack.size, u)
	varstack.push(v)
	v
      }
    }
  }

  def fresh(name: String, u: Boolean): Var = {
    Var(this, "tag:public-cwm-talk@w3.org,e", varstack.size, u)
  }
}

trait N3TheoryBuilder extends N3TermBuilder  {
  type Node = logic1.Term
  type V = Var
  type Label = Name
  type Literal = Ground

  def theory: List[Implication] = scopes.top.theory.reverse.toList

  override def fresh(hint: String, u: Boolean) = scopes.top.fresh(hint, u)
  override def byName(name: Label, u: Boolean) = scopes.top.byName(name.n, u)

  override def uri(i: String) = Name(i)

  override def constant(value: Boolean) = BoolT(value)
  override def constant(value: Int) = IntT(value)
  override def constant(value: Double) = DoubleT(value)
  override def constant(value: BigDecimal) = DecimalT(value)
  override def typed(lex: String, dt: String) = {
    App('data, List(StringT(lex), Name(dt)))
  }
  override def plain(lex: String, lang: Option[Symbol]) = {
    lang match {
      case None => StringT(lex)
      case Some(code) => App('text, List(StringT(lex), StringT(code.name)))
    }
  }

  override def pushScope() = {
    scopes.push(new Scope)
  }

  override def popScope() = {
    assert(scopes.size > 1)
    val terms = scopes.top.theory.map(reify(_)).toList
    scopes.pop()
    terms match {
      case t :: Nil => t
      case _ => App('AND, terms)
    }
  }

  override def addStatement(s: Term, p: Term, o: Term) = {
    val fmla = if (p == log_implies) {
      Implication(t2and(s), t2and(o))
    } else {
      Atomic('related, List(s, p, o))
    }
    scopes.top.theory.push(fmla)
  }

  override def mkList(items: List[Term]) = App('list, items)

  protected def reify(f: Implication): Term = {
    f match {
      case Implies(c, d) => App('IMPLIES, List(reify(c), reify(d)))
      case Or(ei) => App('OR, ei.map(reify _))
      // TODO: Exists??!?!
      case Exists(xi, c) => App('exists, List(App('OOPS, xi.toList), reify(c)))
      case And(ai) => App('AND, ai.map(reify _))
      case Atomic('notnil, List(a)) => a
      case Atomic('related, List(s, p, o)) => App('related, List(s, p, o))
      case Atomic(rel, args) => App('TODO_OOPS, List(App(rel, args)))
    }
  }

  protected def t2and(t: Term): Conjunction = {
    t match {
      case App('AND, args) => Conjunction(args.map(t2atom _))
      case t => Atomic('notnil, List(t))
    }
  }

  protected def t2atom(t: Term): Atomic = {
    t match {
      case App('related, List(s, p, o)) => Atomic('related, List(s, p, o))
      case t => Atomic('notnil, List(t))
    }
  }

  import scala.collection.mutable
  protected val scopes = new mutable.Stack[Scope]
  scopes.push(new Scope)
}

class N3Parser(b: String) extends N3Syntax(b) with N3TheoryBuilder
