package websnowball
package parser

// see http://snowballstem.org/compiler/snowman.html
object Snowball {

  import fastparse.noApi._
  import Lexicals._
  import white._

  val declaration: P[Declaration] = P(
    (keyword("strings") ~/ "(" ~ name.rep(min = 0).map(Strings(_)) ~ ")")
      | (keyword("integers") ~/ "(" ~ name.rep(min = 0).map(Integers(_)) ~ ")")
      | (keyword("booleans") ~/ "(" ~ name.rep(min = 0).map(Booleans(_)) ~ ")")
      | (keyword("routines") ~/ "(" ~ name.rep(min = 0).map(Routines(_)) ~ ")")
      | (keyword("externals") ~/ "(" ~ name.rep(min = 0).map(Externals(_)) ~ ")")
      | (keyword("groupings") ~/ "(" ~ name.rep(min = 0).map(Groupings(_)) ~ ")"))

  def routine(openescape: Char, closeescape: Char, defines: Map[String, String]): P[Routine] = P(name ~ keyword("as") ~/ command(openescape, closeescape, defines)).map(Routine.tupled)

  object i {

    val atom: P[IExpr] = P(
      ("(" ~/ expr ~ ")")
        | ("-" ~/ atom).map(IExpr.Neg(_))
        | (keyword("maxint") ~ PassWith(IExpr.MaxInt))
        | (keyword("minint") ~ PassWith(IExpr.MinInt))
        | (keyword("cursor") ~ PassWith(IExpr.Cursor))
        | (keyword("limit") ~ PassWith(IExpr.Limit))
        | (keyword("size") ~ PassWith(IExpr.Size))
        | (keyword("sizeof") ~/ name.map(IExpr.Sizeof(_)))
        | (keyword("len") ~ PassWith(IExpr.Len))
        | (keyword("lenof") ~/ name.map(IExpr.Lenof(_)))
        | name.map(IExpr.Name(_))
        | int.map(IExpr.Literal(_)))

    val product: P[IExpr] = P(atom ~/ ("*" ~ product.map(r => (l: IExpr) => IExpr.Star(l, r)) | "/" ~ product.map(r =>
      (l: IExpr) => IExpr.Slash(l, r))).?).map {
      case (l, Some(f)) => f(l)
      case (l, None)    => l
    }

    val expr: P[IExpr] = P(product ~/ ("+" ~ expr.map(r => (l: IExpr) => IExpr.Plus(l, r)) | "-" ~ expr.map(r =>
      (l: IExpr) => IExpr.Minus(l, r))).?).map {
      case (l, Some(f)) => f(l)
      case (l, None)    => l
    }

    val command: P[ICommand] = P(
      ref ~ (
        "==" ~/ expr.map(e => (ref: String) => ICommand.Eq(ref, e))
          | "!=" ~/ expr.map(e => (ref: String) => ICommand.Neq(ref, e))
          | "<=" ~/ expr.map(e => (ref: String) => ICommand.Le(ref, e))
          | "<" ~/ expr.map(e => (ref: String) => ICommand.Lt(ref, e))
          | ">=" ~/ expr.map(e => (ref: String) => ICommand.Ge(ref, e))
          | ">" ~/ expr.map(e => (ref: String) => ICommand.Gt(ref, e))
          | "=" ~/ expr.map(e => (ref: String) => ICommand.Assign(ref, e, None))
          | "+=" ~/ expr.map(e => (ref: String) => ICommand.Assign(ref, e, Some(IOp.Plus)))
          | "-=" ~/ expr.map(e => (ref: String) => ICommand.Assign(ref, e, Some(IOp.Minus)))
          | "*=" ~/ expr.map(e => (ref: String) => ICommand.Assign(ref, e, Some(IOp.Star)))
      )).map {
      case (r, f) => f(r)
    }

  }

  def atom(openescape: Char, closeescape: Char, defines: Map[String, String]): P[Command] = P(
    "(" ~/ command(openescape, closeescape, defines).rep(min = 0).map(Command.Group(_)) ~ ")"
      | keyword("not") ~/ command(openescape, closeescape, defines).map(Command.Not(_))
      | keyword("test") ~/ command(openescape, closeescape, defines).map(Command.Test(_))
      | keyword("try") ~/ command(openescape, closeescape, defines).map(Command.Try(_))
      | keyword("do") ~/ command(openescape, closeescape, defines).map(Command.Do(_))
      | keyword("fail") ~/ command(openescape, closeescape, defines).map(Command.Fail(_))
      | keyword("goto") ~/ command(openescape, closeescape, defines).map(Command.Goto(_))
      | keyword("gopast") ~/ command(openescape, closeescape, defines).map(Command.Gopast(_))
      | keyword("repeat") ~/ command(openescape, closeescape, defines).map(Command.Repeat(_))
      | keyword("loop") ~/ (i.expr ~ command(openescape, closeescape, defines)).map(Command.Loop.tupled)
      | keyword("atleast") ~/ (i.expr ~ command(openescape, closeescape, defines)).map(Command.Atleast.tupled)
      | nameOrLiteral(openescape, closeescape, defines)
      | "=>" ~/ name.map(Command.Imply(_))
      | "=" ~/ nameOrLiteral(openescape, closeescape, defines).map(Command.Eq(_))
      | keyword("insert") ~/ nameOrLiteral(openescape, closeescape, defines).map(Command.Insert(_))
      | keyword("attach") ~/ nameOrLiteral(openescape, closeescape, defines).map(Command.Attach(_))
      | "<-" ~/ nameOrLiteral(openescape, closeescape, defines).map(Command.Left(_))
      | keyword("delete") ~/ PassWith(Command.Delete)
      | keyword("hop") ~/ i.expr.map(Command.Hop(_))
      | keyword("next") ~/ PassWith(Command.Next)
      | "[" ~/ PassWith(Command.LBracket)
      | "]" ~/ PassWith(Command.RBracket)
      | "->" ~/ name.map(Command.Right(_))
      | keyword("setmark") ~/ name.map(Command.Setmark(_))
      | keyword("tomark") ~/ i.expr.map(Command.Tomark(_))
      | keyword("atmark") ~/ i.expr.map(Command.Atmark(_))
      | keyword("tolimit") ~/ PassWith(Command.Tolimit)
      | keyword("atlimit") ~/ PassWith(Command.Atlimit)
      | keyword("setlimit") ~/ (command(openescape, closeescape, defines) ~ keyword("for") ~/ command(openescape, closeescape, defines)).map(Command.Setlimit.tupled)
      | keyword("backwards") ~/ command(openescape, closeescape, defines).map(Command.Backwards(_))
      | keyword("reverse") ~/ command(openescape, closeescape, defines).map(Command.Reverse(_))
      | keyword("substring") ~/ PassWith(Command.Substring)
      | keyword("among") ~/ "(" ~/ ((string(openescape, closeescape, defines) ~ name.?).map(Command.Possibility.Literal.tupled) | "(" ~/ command(openescape, closeescape, defines).rep(min = 1).map(Command.Possibility.C(_)) ~ ")").rep(min = 0).map(Command.Among(_)) ~ ")"
      | keyword("set") ~/ name.map(Command.Set(_))
      | keyword("unset") ~/ name.map(Command.Unset(_))
      | keyword("non") ~ ("-".!.?.map(_.isDefined) ~ name).map(Command.Non.tupled)
      | name.map(Command.Name(_))
      | keyword("true") ~/ PassWith(Command.True)
      | keyword("false") ~/ PassWith(Command.False)
      | "?" ~/ PassWith(Command.Debug)
      | NoCut(ref ~ command(openescape, closeescape, defines)).map(Command.StringC.tupled)
      | i.command)

  def and(openescape: Char, closeescape: Char, defines: Map[String, String]): P[Command] = P(atom(openescape, closeescape, defines) ~/ (keyword("and") ~ and(openescape, closeescape, defines)).?).map {
    case (l, Some(r)) => Command.And(l, r)
    case (l, None)    => l
  }

  def command(openescape: Char, closeescape: Char, defines: Map[String, String]): P[Command] = P(and(openescape, closeescape, defines) ~/ (keyword("or") ~ command(openescape, closeescape, defines)).?).map {
    case (l, Some(r)) => Command.Or(l, r)
    case (l, None)    => l
  }

  private def nameOrLiteral(openescape: Char, closeescape: Char, defines: Map[String, String]): P[NameOrLiteral] = P(
    name.map(NameOrLiteral.Name(_))
      | string(openescape, closeescape, defines).map(NameOrLiteral.Literal(_)))

  private val plusOrMinus: P[Boolean] = P("+" ~/ PassWith(true) | "-" ~/ PassWith(false))

  def grouping(openescape: Char, closeescape: Char, defines: Map[String, String]): P[Grouping] = P(name ~ nameOrLiteral(openescape, closeescape, defines) ~ (plusOrMinus ~ nameOrLiteral(openescape, closeescape, defines)).rep(min = 0)).map(Grouping.tupled)

  def definition(openescape: Char, closeescape: Char, defines: Map[String, String]): P[Definition] =
    P(
      keyword("define") ~/
        (routine(openescape, closeescape, defines) | grouping(openescape, closeescape, defines))
      | (keyword("backwardmode") ~/ "(" ~ p(openescape, closeescape, defines) ~ ")").map(Backwardmode(_)))

  val program: P[Program] = ws ~ p('{', '}', Map.empty) ~ ws ~ End

  private def defs(openescape: Char, closeescape: Char, defines: Map[String, String]): P[Seq[Definition]] = {
    lazy val d: P[Definition] = definition(openescape, closeescape, defines)
    lazy val p: P[Seq[Definition]] = P((keyword("stringescapes") ~/ AnyChar.! ~ AnyChar.!).flatMap {
      case (open, close) =>
        ws ~ defs(open(0), close(0), defines)
    }
      | (keyword("stringdef") ~/ printingname ~ string(openescape, closeescape, defines)).flatMap {
        case (name, s) =>
          ws ~ defs(openescape, closeescape, defines.updated(name, s))
      }
      | (d ~ p).map {
        case (head, tail) => head +: tail
      }
      | PassWith(Seq.empty))
    p
  }

  private def p(openescape: Char, closeescape: Char, defines: Map[String, String]): P[Program] =
    P(declaration.rep(min = 0) ~ defs(openescape, closeescape, defines)).map(Program.tupled)

}
