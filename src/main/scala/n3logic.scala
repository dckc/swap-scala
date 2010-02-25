package org.w3.swap.logic1n3

import java.math.BigDecimal

import org.w3.swap
import swap.logic1
import swap.logic1.{Term, FunctionTerm, Variable}
import Term.Subst
import swap.logic1cl.{Conjunction, Implication, Disjunction, Existential,
		      Implies, Exists, And, Or, Atomic}
import swap.rdf.{RDFGraphParts}
import swap.rdflogic.{Name, Ground}
import swap.n3.{N3Syntax, N3TermBuilder}


case class Var(i: String, n: Int, universal: Boolean) extends Variable
case class BoolT(v: Boolean) extends Ground
case class IntT(v: Int) extends Ground
case class StringT(v: String) extends Ground
case class DoubleT(v: Double) extends Ground
case class DecimalT(v: BigDecimal) extends Ground

abstract class Application extends FunctionTerm

case class App(sym: Symbol, params: List[Term]) extends Application  {
  override def fun = sym
  override def args = params
  override def subst(s: Subst) = App(sym, params.map(_.subst(s)))
}

case class AppNamed(sym: Symbol,
		    keys: List[Symbol],
		    values: List[Term]) extends Application {
  override def fun = sym :: keys
  override def args = values
  override def subst(s: Subst) = AppNamed(sym, keys, values.map(_.subst(s)))
}

case class Quant(sym: Symbol, vars: List[Variable],
		 t: Term) extends FunctionTerm  {
  override def fun = (sym, vars)
  override def args = List(t)
  override def subst(s: Subst) = {
    // TODO: think about intersection case
    assert((s.keySet intersect vars.toSet).isEmpty)
    Quant(sym, vars, t.subst(s))
  }
}


class Scope {
  import scala.collection.mutable
  val varstack = new mutable.Stack[Var]

  val theory = new mutable.Stack[Implication]

  def byName(name: String, u: Boolean): Var = {
    varstack.find { v => v.i == name } match {
      case Some(v) => v
      case None => {
	val v = Var(name, varstack.size, u)
	varstack.push(v)
	v
      }
    }
  }

  def fresh(name: String, u: Boolean): Var = {
    Var("tag:public-cwm-talk@w3.org,e", varstack.size, u)
  }
}

/**
 * TODO: this FormulaAsTerm stuff is kinda nutso... get rid of it somehow?
 */
object FormulaAsTerm {
  def related(s: Term, p: Term, o: Term): Term = {
    App('related, List(s, p, o))
  }

  def statement(s: Term, p: Term, o: Term): Atomic = {
    Atomic('holds, List(s, p, o))
  }

  def termatom(t: Term): Atomic = {
    Atomic('notnil, List(t))
  }

  def reify(f: Implication): Term = {
    f match {
      case Implies(c, d) => App('IMPLIES, List(reify(c), reify(d)))
      case Or(ei) => App('OR, ei.map(reify _))
      case Exists(xi, c) => Quant('exists, xi.toList, reify(c))
      case And(ai) => App('AND, ai.map(reify _))
      case Atomic('holds, List(s, p, o)) => related(s, p, o)
      case Atomic('notnil, List(a)) => a
      case Atomic(rel, args) => App('TODO_OOPS, List(App(rel, args)))
    }
  }

  def theoryatom(fmlas: Iterable[Implication]): Atomic = {
    termatom(App('AND, fmlas.map(reify _).toList))
  }

  def t2or(t: Term): Disjunction = {
    t match {
      case App('OR, args) => Disjunction(args.map(t2exi _))
      case _ => t2exi(t)
    }
  }

  def t2exi(t: Term): Existential = {
    t match {
      case Quant('exists, vars, t) => Exists(vars.toSet, t2and(t))
      case _ => t2and(t)
    }
  }

  def t2and(t: Term): Conjunction = {
    t match {
      case App('AND, args) => Conjunction(args.map(t2atom _))
      case _ => t2atom(t)
    }
  }

  def t2atom(t: Term): Atomic = {
    t match {
      case App('related, List(s, p, o)) => Atomic('holds, List(s, p, o))
      case t => termatom(t)
    }
  }

}

trait N3TheoryBuilder extends N3TermBuilder  {
  import FormulaAsTerm._

  type Node = logic1.Term
  type V = Var
  type Label = Name
  type Literal = Ground

  def theory: List[Implication] = {
    val uvars = scopes.top.varstack.filter { _.universal }
    if (!uvars.isEmpty && scopes.size > 1) {
      throw new Exception("TODO: Forall inside nested scope.")
    }

    val theory1 = scopes.top.theory.reverse
    val evars: Set[Variable] = scopes.top.varstack.filter(!_.universal).toSet
    if (evars.isEmpty) theory1.toList
    else {
      /*
       * Collect atoms under existential quantifiers as appropriate.
       * a b _:c. d e _:c. becomes: Exists(c, And(...))
       */
      val imps = theory1.partialMap { case i: Implies => i }
      val ors = theory1 partialMap {
	case Or(fmlas) =>
	  Or(fmlas map { f => Existential(f.xi union evars, f.c) })
      }
      val atoms = theory1.toList.flatMap {
	case And(atoms) => atoms
	case a: Atomic => List(a)
	case f: Or => Nil // handled above
	case f: Implies => Nil // handled above
	case f: Exists => throw new Exception("addStatemnt is broken: " + f)
      }

      List(Existential(evars, Conjunction(atoms))) ++ imps ++ ors
    }
  }


  override def fresh(hint: String) = scopes.top.fresh(hint, false)
  override def byName(name: String) = scopes.top.byName(name, false)
  override def universal(name: String) = {
    scopes.last.byName(name, true)
  }
  override def declare(name: String) = {
    scopes.top.byName(name, true)
  }

  /**
   * If there's a variable by the name i, return it; else
   * make a logical name.
   */
  override def uri(i: String) = {
    val vars = for {
      scope <- scopes
      v <- scope.varstack
      if v.i == i
    } yield v

    if (vars.isEmpty) Name(i)
    else vars.head
  }

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

  override def addStatement(s: Term, p: Term, o: Term) = {
    val fmla = if (p == log_implies) {
      Implication(t2and(s), t2or(o))
    } else if (p == log_or ) {
      Disjunction(List(t2exi(s), t2exi(o)))
    } else {
      Atomic('holds, List(s, p, o))
    }
    scopes.top.theory.push(fmla)
  }

  // TODO: use 'cons rather than 'list?
  override def mkList(items: List[Term]) = App('list, items)

  override def popScope() = {
    assert(scopes.size > 1)
    val terms = this.theory.map(reify(_)).toList
    scopes.pop()
    terms match {
      case t :: Nil => t
      case _ => App('AND, terms)
    }
  }

  import scala.collection.mutable
  protected val scopes = new mutable.Stack[Scope]
  scopes.push(new Scope)
}

class N3Parser(b: String) extends N3Syntax(b) with N3TheoryBuilder

import swap.rif.RIFBuilder
import swap.rifxml.RIFBLDXML

/**
 * OWL vocabulary... TODO: move this where it belongs.
 */
object OWL {
  final val ns = "http://www.w3.org/2002/07/owl#"
  final val sameAs = ns + "sameAs"
  final val differentFrom = ns + "differentFrom"
}

trait N3RIF extends RIFBuilder {
  import FormulaAsTerm._

  override type Term = logic1.Term
  type BaseTerm = logic1.Term
  type ArgTerm = Application
  type Var = swap.logic1n3.Var
  type ArgNames = Symbol
  type FunctionSymbol = Symbol
  type PredicateSymbol = Symbol

  type Formula = Implication // TODO: GroupFormula, sorta like a Theory
  type ConditionFormula = Disjunction
  type ConjunctionOfAtoms = Conjunction
  override type AtomicFormula = Atomic
  type RuleImplication = Implication
  type Rule = Implication

  override def data(lex: String, dt: String) = {
    App('data, List(StringT(lex), Name(dt)))
  }

  override def positional_term(fun: FunctionSymbol,
			       args: Seq[BaseTerm]): ArgTerm = {
    App(fun, args.toList)
  }

  override def named_arguments_term(t: FunctionSymbol,
				    args: Map[ArgNames, BaseTerm]): ArgTerm = {
    AppNamed(t, args.keysIterator.toList, args.valuesIterator.toList)
  }

  override def closed_list(items: List[Term]): Term = App('list, items)
  //TODO: def open_list(items: List[Term], tail: Term): Term
  //TODO: def external_term(t: ArgTerm): Term = t

  protected val sameAs = Name(OWL.sameAs)
  protected val differentFrom = Name(OWL.differentFrom)
  override def equality_term(t: BaseTerm, s: BaseTerm): AtomicFormula = {
    statement(t, sameAs, s)
  }

  protected val rdf_type = Name(swap.rdf.Vocabulary.`type`)
  protected val rdf_nil = Name(swap.rdf.Vocabulary.nil)
  override def membership_term(t: BaseTerm, s: BaseTerm): AtomicFormula = {
    statement(t, rdf_type, s)
  }

  protected val subClassOf =
    Name("http://www.w3.org/2000/01/rdf-schema#subClassOf")
  override def subclass_term(t: BaseTerm, s: BaseTerm): AtomicFormula = {
    statement(t, subClassOf, s)
  }

  override def frame_term(s: BaseTerm,
			  args: Map[BaseTerm, BaseTerm]): AtomicFormula = {
    val statements = args.map { case (p, o) => statement(s, p, o) } toList;
    termatom(reify(Conjunction(statements)))
  }

  override def atomic_formula(p: PredicateSymbol,
		     args: Seq[BaseTerm]): AtomicFormula = {
    Atomic(p, args.toList)
  }
  //TODO: def external_atomic(a: AtomicFormula): AtomicFormula = a

  /* oops... coherent conjunction can only take atoms, but RIF
   * allows nested And, Or, and Exists
  override def conjunction(phi1n: Seq[ConditionFormula]): ConditionFormula = {
    Conjunction(phi1n.toList)
  }

  override def disjunction(phi1n: Seq[ConditionFormula]): ConditionFormula = {
    Disjunction(phi1n.toList)
  }

  override def existential(v1n: Set[Var],
			   phi: ConditionFormula): ConditionFormula = {
    Existential(v1n map { v => v : Variable }, phi)
  }
  */

  /**
   * @param phi: none of the atomic formulas in phi is
   *             an externally defined term
   */
  override def implication(phi: ConjunctionOfAtoms,
		  psi: ConditionFormula): RuleImplication = {
    Implication(phi, psi)
  }

  /**
   * @param phi: constraint: freevars(phi) subset v1n
   */
  override def universal_rule(v1n: Set[Var], phi: RuleImplication): Rule = phi

  /**
   * @param phi: constraint: freevars(phi) subset v1n
   */
  override def universal_fact(v1n: Set[Var], phi: AtomicFormula) = phi

  /**
   * If phi1, ..., phin are RIF-BLD rules, universal facts,
   * variable-free rule implications, variable-free atomic formulas,
   * or group formulas then Group(phi1 ... phin) is a group formula.
   * As a special case, the empty group formula, Group(), is allowed
   * and is treated as a tautology, i.e., a formula that is always true. 
   */
  override def group(phi1n: Seq[Formula]): Formula = theoryatom(phi1n)

  override def document(base: Option[String],
			prefix_directives: Seq[(String, String)],
			import_directives: Seq[(String, Option[String])],
			gamma: Option[Formula]): Formula = {
    val baseaddr = base getOrElse "data:"
    val web = new swap.webdata.BaseOpener(baseaddr)

    val import_theory: Iterable[Formula] = import_directives flatMap {
      case (loc, profile) =>  {
	// TODO: check profile
	val p = new N3Parser(baseaddr)
	val (content, meta) = web.open(loc, swap.webdata.WebData.N3)

	p.parseAll(p.document, content) match {
	  case p.Success(_, _) => p.theory
	  case lose => {
	    System.err.println("N3Parser failed:")
	    System.err.println(lose.toString)
	    List(statement(rdf_nil, differentFrom, rdf_nil)) //or: List(Or(Nil))
	  }
	}
      }
    }

    theoryatom(import_theory ++ gamma)
  }

}

class RIFParser extends RIFBLDXML with N3RIF
