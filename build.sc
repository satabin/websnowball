import mill._
import mill.scalalib._
import mill.scalalib.scalafmt._

import coursier.maven.MavenRepository

object websnowball extends ScalaModule with ScalafmtModule {

  def scalaVersion = "2.12.6"

  def repositories = super.repositories ++ Seq(
		MavenRepository("https://oss.sonatype.org/content/repositories/snapshots"),
		MavenRepository("https://oss.sonatype.org/content/repositories/snapshots"))

  def ivyDeps = Agg(
    ivy"com.lihaoyi::fastparse:1.0.0",
    ivy"org.gnieh::swam-core:0.1.0-SNAPSHOT")

}
