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
package parser

import fastparse._
import fastparse.all._

object Lexicals {

  val comment: P0 =
    P(linecomment | blockcomment)

  val linecomment: P0 =
    P("//" ~/ CharsWhile(_ != '\u000a', min = 0))

  val blockcomment: P0 =
    P("/*" ~/ (CharsWhile(!"*/".contains(_)) | !"*/" ~ "*" | !"/*" ~ "/" | blockcomment).rep(min = 0) ~ "*/")

  val ws: P0 =
    P((CharIn(" \u0009\u000a\u000d") | comment).rep(min = 0))

  val white = WhitespaceApi.Wrapper {
    NoTrace(ws)
  }

  private val namePart: P0 = P(CharIn('a' to 'z', 'A' to 'Z', '0' to '9', "_"))

  val name: P[String] =
    P(CharIn('a' to 'z', 'A' to 'Z') ~ namePart.rep(min = 0)).!.filter(!reserved.contains(_)).opaque("name")

  val ref: P[String] = P("$" ~ name)

  val printingname: P[String] =
    P(CharsWhile(c => !(Character.isWhitespace(c) || Character.isISOControl(c)), min = 1)).!.opaque(
      "printing character sequence")

  val int: P[Int] = P(CharsWhileIn('0' to '9', min = 1).!.map(_.toInt))

  private val hexdigit: P0 = P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))

  def string(openescape: Char, closeescape: Char, defines: Map[String, String]): P[String] =
    P {
      val open = openescape.toString
      val close = closeescape.toString
      "'" ~ (
        CharsWhile(!Set('\'', openescape).contains(_), min = 1).!
          | (open ~ ("U+" ~ hexdigit
            .rep(min = 1)
            .!
            .map(Integer.parseInt(_, 16).toChar)
            | CharsWhile(_ != closeescape).!.filter(defines.contains(_)).map(defines(_))
            | AnyChar.!) ~ close)
      ).rep(min = 0).map(_.mkString) ~ "'"
    }.opaque("string")

  def keyword(k: String): P0 = P(k ~ !namePart).opaque(s"`$k`")

  val reserved =
    Set(
      "integers",
      "booleans",
      "routines",
      "externals",
      "groupings",
      "stringescapes",
      "stringdef",
      "define",
      "as",
      "externals",
      "maxint",
      "minint",
      "cursor",
      "limit",
      "size",
      "sizeof", /* "len", "lenof", */ "or",
      "and",
      "not",
      "test",
      "try",
      "do",
      "fail",
      "goto",
      "gopast",
      "repeat",
      "loop",
      "atleast",
      "insert",
      "attach",
      "delete",
      "hop",
      "next",
      "setmark",
      "tomark",
      "atmark",
      "tolimit",
      "atlimit",
      "setlimit",
      "for",
      "backwards",
      "reverse",
      "substring",
      "among",
      "set",
      "unset",
      "non",
      "true",
      "false",
      "backwardmode"
    )

}
