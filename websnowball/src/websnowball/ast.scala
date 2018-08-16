package websnowball

sealed trait Declaration
case class Strings(names: Seq[String]) extends Declaration
case class Integers(names: Seq[String]) extends Declaration
case class Booleans(names: Seq[String]) extends Declaration
case class Routines(names: Seq[String]) extends Declaration
case class Externals(names: Seq[String]) extends Declaration
case class Groupings(names: Seq[String]) extends Declaration

sealed trait Definition

case class Backwardmode(p: Program) extends Definition

case class Routine(name: String, command: Command) extends Definition

sealed trait Command
object Command {

  case class Group(commands: Seq[Command]) extends Command

  case class And(left: Command, right: Command) extends Command

  case class Or(left: Command, right: Command) extends Command

  case class StringC(name: String, c: Command) extends Command

  case class Not(command: Command) extends Command

  case class Test(command: Command) extends Command

  case class Do(command: Command) extends Command

  case class Try(command: Command) extends Command

  case class Fail(command: Command) extends Command

  case class Goto(command: Command) extends Command

  case class Gopast(command: Command) extends Command

  case class Repeat(command: Command) extends Command

  case class Loop(e: IExpr, command: Command) extends Command

  case class Atleast(e: IExpr, command: Command) extends Command

  case class Eq(s: NameOrLiteral) extends Command

  case class Insert(s: NameOrLiteral) extends Command

  case class Attach(s: NameOrLiteral) extends Command

  case class Left(s: NameOrLiteral) extends Command

  case object Delete extends Command

  case class Hop(e: IExpr) extends Command

  case object Next extends Command

  case class Imply(name: String) extends Command

  case object LBracket extends Command

  case object RBracket extends Command

  case class Right(name: String) extends Command

  case class Setmark(name: String) extends Command

  case class Tomark(e: IExpr) extends Command

  case class Atmark(e: IExpr) extends Command

  case object Tolimit extends Command

  case object Atlimit extends Command

  case class Setlimit(c1: Command, c2: Command) extends Command

  case class Backwards(command: Command) extends Command

  case class Reverse(command: Command) extends Command

  case object Substring extends Command

  case class Among(list: Seq[Possibility]) extends Command

  sealed trait Possibility

  object Possibility {
    case class C(cs: Seq[Command]) extends Possibility
    case class Literal(s: String, name: Option[String]) extends Possibility
  }

  case class Set(name: String) extends Command

  case class Unset(name: String) extends Command

  case class Name(name: String) extends Command

  case class Non(minus: Boolean, name: String) extends Command

  case object True extends Command

  case object False extends Command

  case object Debug extends Command

}

sealed trait ICommand extends Command
object ICommand {

  case class Assign(name: String, e: IExpr, op: Option[IOp]) extends ICommand

  case class Eq(name: String, e: IExpr) extends ICommand
  case class Neq(name: String, e: IExpr) extends ICommand
  case class Lt(name: String, e: IExpr) extends ICommand
  case class Le(name: String, e: IExpr) extends ICommand
  case class Gt(name: String, e: IExpr) extends ICommand
  case class Ge(name: String, e: IExpr) extends ICommand

}

sealed trait IOp
object IOp {
  case object Plus extends IOp
  case object Minus extends IOp
  case object Star extends IOp
  case object Slash extends IOp
}

sealed trait IExpr
object IExpr {

  case object MaxInt extends IExpr
  case object MinInt extends IExpr
  case object Cursor extends IExpr
  case object Limit extends IExpr
  case object Size extends IExpr
  case class Sizeof(name: String) extends IExpr
  case object Len extends IExpr
  case class Lenof(name: String) extends IExpr
  case class Name(name: String) extends IExpr
  case class Literal(i: Int) extends IExpr
  case class Neg(e: IExpr) extends IExpr

  case class Plus(left: IExpr, right: IExpr) extends IExpr
  case class Minus(left: IExpr, right: IExpr) extends IExpr
  case class Star(left: IExpr, right: IExpr) extends IExpr
  case class Slash(left: IExpr, right: IExpr) extends IExpr

}

case class Grouping(name: String, g: NameOrLiteral, tail: Seq[(Boolean, NameOrLiteral)]) extends Definition

sealed trait NameOrLiteral extends Command

object NameOrLiteral {

  case class Name(name: String) extends NameOrLiteral

  case class Literal(s: String) extends NameOrLiteral

}

case class Program(decls: Seq[Declaration], defs: Seq[Definition])
