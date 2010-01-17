package org.w3.swap.logic

/**
 * cf. <a href="http://en.wikipedia.org/wiki/Conjunctive_query"
 * >Conjunctive query in wikipedia</a>
 * 
 * @param data a function that takes a set of function symbols
 *             and returns the atomic formulas that use those function symbols.
 * TODO: consider supporting relation symbols other than the
 * implicit NotNil.
 * 
 */
abstract class ConjunctiveKB {
  import AbstractSyntax.{Subst, unifyall}

  def getData(tokens: Seq[Any]): Stream[Formula]

  def solve(goal: Formula): Stream[Subst] = solve(goal, Map())

  /**
   * @throws BadGoal if the goal isn't an Atomic
   */
  def solve(goal: Formula, s: Subst): Stream[Subst] = {
    def tokens(terms: Seq[Term]): Seq[Any] = {
      terms.flatMap {case ap: Application => Some(ap.fun); case _ => None}
    }

    //println("solve: checking goal: " + goal)

    goal match {

      case goalatom: Atomic => {
	var facts = getData(tokens(goal.terms()))

	facts.flatMap {
	  case fact: Atomic =>
	    /*@@ should check relation symbol too! */
	    unifyall(fact.terms(), goalatom.terms(), s).toStream
	  case _ => Stream.empty
	}
      }

      case And(fmlas) => solveall(fmlas, s)

      case _ => { throw new BadGoal(goal) }
    }
  }

  def solveall(goals: Iterable[Formula], s: Subst): Stream[Subst] = {
    if (goals.isEmpty) Stream.cons(s, Stream.empty) else {
      solve(goals.head, s).flatMap(si => solveall(goals.tail, si))
    }
  }

}
    
class BadGoal(g: Formula) extends Exception {
  override def toString() = "Bad Goal formula stucture: " + g.toString()
}

