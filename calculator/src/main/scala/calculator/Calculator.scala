package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    println(s"namedExpressions = $namedExpressions")
    namedExpressions.map {
      case (name, signal) =>
        //println(s"name=$name, signal=$signal")
        (name, Signal {
          val v = signal()
          eval(v, namedExpressions)
        })
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v) => v
      case Ref(name) if ! references.contains(name) => Double.NaN
      case Ref(name) if hasCycle(name, references(name)(), Set(name)) => Double.NaN
      case Ref(name) =>
        val newVal = references.getOrElse(name, Signal(Literal(Double.NaN)))()
        eval(newVal, references)
      case Plus(a, b) => eval(a, references) + eval(b, references)
      case Minus(a, b) => eval(a, references) - eval(b, references)
      case Times(a, b) => eval(a, references) * eval(b, references)
      case Divide(a, b) => eval(a, references) / eval(b, references)
    }
  }

  private def hasCycle(name: String, expr: Expr, visited: Set[String]): Boolean = {
    val v = visited + name

    expr match {
      case Literal(_) => false
      case Ref(n) =>
        if (visited.contains(n)) {
          true
        } else {
          n.equals(name)
      }
      case Plus(a, b) => hasCycle(name, a, v) || hasCycle(name, b, v)
      case Minus(a, b) => hasCycle(name, a, v) || hasCycle(name, b, v)
      case Times(a, b) => hasCycle(name, a, v) || hasCycle(name, b, v)
      case Divide(a, b) => hasCycle(name, a, v) || hasCycle(name, b, v)
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
