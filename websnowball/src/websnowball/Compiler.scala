/*
 * Copyright 2018 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package websnowball

import swam._
import swam.syntax._

import cats._
import cats.implicits._

import scala.language.higherKinds

class Compiler[F[_]] {

  private val tsignal = ResultType(Some(ValType.I32))

  private val ctrue = i32.Const(1)

  private val cfalse = i32.Const(0)

  private val break = BrIf(0)

  private val minint = i32.Const(Int.MinValue)

  private val maxint = i32.Const(Int.MaxValue)

  def compile(cs: Seq[Command], env: Environment[F])(implicit F: MonadError[F, Throwable]): F[Seq[Inst]] =
    ???

  def compile(c: Command, env: Environment[F])(implicit F: MonadError[F, Throwable]): F[Seq[Inst]] =
    c match {
      case Command.Group(cs) =>
        for (cs <- compile(cs, env))
          yield Seq(Block(tsignal, cs.toVector), ctrue, i32.Xor, break)
      case Command.And(l, r) =>
        for {
          l <- compile(l, env)
          r <- compile(r, env)
        } yield l ++ Seq(If(tsignal, r.toVector, Vector(cfalse)), ctrue, i32.Xor, break)
      case Command.Or(l, r) =>
        for {
          l <- compile(l, env)
          r <- compile(r, env)
        } yield l ++ Seq(If(tsignal, Vector(ctrue), r.toVector), ctrue, i32.Xor, break)
      case Command.Not(c) =>
        for (c <- compile(c, env))
          yield c :+ break
      case Command.Try(c) =>
        for (c <- compile(c, env))
          yield Seq(Block(tsignal, c.toVector), Drop)
      case Command.Fail(c) =>
        for (c <- compile(c, env))
          yield Seq(Block(tsignal, c.toVector), Drop, Br(0))
      case ICommand.Assign(name, e, None) =>
        for {
          idx <- env.integer(name)
          e <- compile(e, env)
        } yield e :+ SetGlobal(idx)
      case ICommand.Assign(name, e, Some(IOp.Plus)) =>
        for {
          idx <- env.integer(name)
          e <- compile(e, env)
        } yield GetGlobal(idx) +: (e ++ Seq(i32.Add, SetGlobal(idx)))
      case ICommand.Assign(name, e, Some(IOp.Minus)) =>
        for {
          idx <- env.integer(name)
          e <- compile(e, env)
        } yield GetGlobal(idx) +: (e ++ Seq(i32.Sub, SetGlobal(idx)))
      case ICommand.Assign(name, e, Some(IOp.Star)) =>
        for {
          idx <- env.integer(name)
          e <- compile(e, env)
        } yield GetGlobal(idx) +: (e ++ Seq(i32.Mul, SetGlobal(idx)))
      case ICommand.Assign(name, e, Some(IOp.Slash)) =>
        for {
          idx <- env.integer(name)
          e <- compile(e, env)
        } yield GetGlobal(idx) +: (e ++ Seq(i32.DivS, SetGlobal(idx)))
      case ICommand.Eq(l, r) =>
        for {
          l <- compile(l, env)
          r <- compile(r, env)
        } yield l ++ r ++ Seq(i32.Ne, break)
      case ICommand.Neq(l, r) =>
        for {
          l <- compile(l, env)
          r <- compile(r, env)
        } yield l ++ r ++ Seq(i32.Eq, break)
      case ICommand.Lt(l, r) =>
        for {
          l <- compile(l, env)
          r <- compile(r, env)
        } yield l ++ r ++ Seq(i32.GeS, break)
      case ICommand.Le(l, r) =>
        for {
          l <- compile(l, env)
          r <- compile(r, env)
        } yield l ++ r ++ Seq(i32.GtS, break)
      case ICommand.Gt(l, r) =>
        for {
          l <- compile(l, env)
          r <- compile(r, env)
        } yield l ++ r ++ Seq(i32.LeS, break)
      case ICommand.Ge(l, r) =>
        for {
          l <- compile(l, env)
          r <- compile(r, env)
        } yield l ++ r ++ Seq(i32.LtS, break)
    }

  def compile(e: IExpr, env: Environment[F])(implicit F: MonadError[F, Throwable]): F[Seq[Inst]] =
    e match {
      case IExpr.MaxInt     => F.pure(Seq(maxint))
      case IExpr.MinInt     => F.pure(Seq(minint))
      case IExpr.Cursor     => F.pure(Seq(GetGlobal(env.cursor)))
      case IExpr.Limit      => F.pure(Seq(GetGlobal(env.limit)))
      case IExpr.Literal(i) => F.pure(Seq(i32.Const(i)))
      case IExpr.Name(n) =>
        for (idx <- env.integer(n))
          yield Seq(GetGlobal(idx))
      case IExpr.Plus(l, r) =>
        for {
          l <- compile(l, env)
          r <- compile(r, env)
        } yield l ++ r :+ i32.Add
      case IExpr.Minus(l, r) =>
        for {
          l <- compile(l, env)
          r <- compile(r, env)
        } yield l ++ r :+ i32.Sub
      case IExpr.Star(l, r) =>
        for {
          l <- compile(l, env)
          r <- compile(r, env)
        } yield l ++ r :+ i32.Mul
      case IExpr.Slash(l, r) =>
        for {
          l <- compile(l, env)
          r <- compile(r, env)
        } yield l ++ r :+ i32.DivS
      case IExpr.Neg(e) =>
        for (e <- compile(e, env))
          yield i32.Const(0) +: e :+ i32.Sub
      case IExpr.Size      => F.raiseError(new Exception("not implemented"))
      case IExpr.Sizeof(s) => F.raiseError(new Exception("not implemented"))
      case IExpr.Len       => F.raiseError(new Exception("not implemented"))
      case IExpr.Lenof(s)  => F.raiseError(new Exception("not implemented"))
    }

}
