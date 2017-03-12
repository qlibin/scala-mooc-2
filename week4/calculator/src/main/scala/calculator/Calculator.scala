package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
    namedExpressions.map {
      case (name, expr) => (name, Signal(eval(getReferenceExpr(name, namedExpressions), namedExpressions)))
    }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double =
    eval_(expr, references, Set())

  private def eval_(expr: Expr, references: Map[String, Signal[Expr]], evaluated: Set[Expr]): Double =
    if (evaluated.contains(expr)) Double.NaN
    else expr match {
      case Literal(value) => value
      case Ref(name) => val refExpr = getReferenceExpr(name, references)
          eval_(refExpr, references, evaluated + expr)
      case Plus(a, b) =>
        eval_(a, references, evaluated) + eval_(b, references, evaluated)
      case Minus(a, b) =>
        eval_(a, references, evaluated) - eval_(b, references, evaluated)
      case Times(a, b) =>
        eval_(a, references, evaluated) * eval_(b, references, evaluated)
      case Divide(a, b) =>
        eval_(a, references, evaluated) / eval_(b, references, evaluated)
    }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]): Expr = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
