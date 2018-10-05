
package edu.ucsb.cs.cs162.tuts.calculator

// A mathematical expression.
sealed trait Expr

// A variable expression with a name.
final case class Var(name: String) extends Expr

// A number expression with a numeric value.
final case class Num(value: Double) extends Expr

// A unary operation expression (eg. -5 is UnOp("-", Num(5))).
final case class UnOp(op: String, value: Expr) extends Expr

// A binary operation expression (eg. 2+3 is BinOp("+", Num(2), Num(3))))
final case class BinOp(op: String, left: Expr, right: Expr) extends Expr

// The calculator object.
object Calculator {

  // Simplifies the head of the expression (should not simplify recursively!).
  def simplifyHead(expr: Expr): Expr = {
    expr match {
      case UnOp("-", x) => x match {
        case Var(name) => Var("-" + name)

        case Num(value) => Num(-1 * value)

        case UnOp("-", y) => y
      }

      case BinOp("+", Num(0), x) => x

      case BinOp("+", x, Num(0)) => x

      case BinOp("*", Num(1), x) => x

      case BinOp("*", x, Num(1)) => x

      case BinOp("*", Num(0), x) => Num(0)

      case BinOp("*", x, Num(0)) => Num(0)

      case BinOp("-", x, y) => Num((x == y).compare(true))

      case BinOp(op, Num(x), Num(y)) => op match {
        case "+" => Num(x + y)

        case "-" => Num(x - y)

        case "*" => Num(x * y)
      }
    }
  }

  // Evaluates the expression to a numeric value.
  def evaluate(expr: Expr): Double = expr match {
    case Num(value) => value

    case Var(name) => 1

    case UnOp(op, value) => op match {
      case "-" => -evaluate(value)

      case _ => 1
    }

    case BinOp(op, x, y) => {
      if (x == Var("DUP")) {
        op match {
          case "-" => evaluate(y) - evaluate(y)

          case "+" => evaluate(y) + evaluate(y)

          case "*" => evaluate(y) * evaluate(y)

          case _ => 1
        }
      }
      else if (y == Var("DUP")) {
        op match {
          case "-" => evaluate(x) - evaluate(x)

          case "+" => evaluate(x) + evaluate(x)

          case "*" => evaluate(x) * evaluate(x)

          case _ => 1
        }
      }
      else {
        op match {
          case "-" => evaluate(x) - evaluate(y)

          case "+" => evaluate(x) + evaluate(y)

          case "*" => evaluate(x) * evaluate(y)

          case _ => 1
        }
      }
    }
  }
}
