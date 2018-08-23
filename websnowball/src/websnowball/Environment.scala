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

import swam.syntax._

import cats._

import scala.language.higherKinds

case class Environment[F[_]](strings: Seq[String] = Seq.empty,
                             integers: Seq[String] = Seq.empty,
                             booleans: Seq[String] = Seq.empty,
                             routines: Seq[String] = Seq.empty,
                             externals: Seq[String] = Seq.empty,
                             compiled: Map[String, Expr]) {

  val cursor = 0

  val limit = 1

  def integer(name: String)(implicit F: MonadError[F, Throwable]): F[Int] = {
    val idx = integers.indexOf(name)
    if (idx >= 0)
      F.pure(idx + 2)
    else
      F.raiseError(new Exception(s"unknown integer $name"))
  }

  def string(name: String)(implicit F: MonadError[F, Throwable]): F[Int] = {
    val idx = strings.indexOf(name)
    if (idx >= 0)
      F.pure(idx + 2)
    else
      F.raiseError(new Exception(s"unknown string $name"))
  }

}
