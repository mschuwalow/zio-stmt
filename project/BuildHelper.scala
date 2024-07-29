import sbt._
import Keys._
import scalafix.sbt.ScalafixPlugin.autoImport._

object BuildHelper {
  final val Scala213 = "2.13.14"
  final val Scala3   = "3.4.2"

  def extraCompilerOptions = Seq(
    scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _))  =>
        List("-language:implicitConversions", "-Xignore-scala2-macros")
      case Some((2, 13)) =>
        List("-Ywarn-unused:params,-implicits") ++ extra2xCompilerOptions
      case Some((2, 12)) =>
        List(
          "-opt-warnings",
          "-Ywarn-extra-implicit",
          "-Ywarn-unused:_,imports",
          "-Ywarn-unused:imports",
          "-Ypartial-unification",
          "-Yno-adapted-args",
          "-Ywarn-inaccessible",
          "-Ywarn-infer-any",
          "-Ywarn-nullary-override",
          "-Ywarn-nullary-unit",
          "-Ywarn-unused:params,-implicits",
          "-Xfuture",
          "-Xsource:2.13",
          "-Xmax-classfile-name",
          "242"
        ) ++ extra2xCompilerOptions
      case _             => Nil
    })
  )

  private val extra2xCompilerOptions =
    List(
      "-language:higherKinds",
      "-language:existentials",
      "-explaintypes",
      "-Yrangepos",
      "-Xlint:_,-missing-interpolator,-type-parameter-shadow",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard"
    )

}

// import sbt._
// import Keys._
// import sbtbuildinfo._
// import BuildInfoKeys._
// import scalafix.sbt.ScalafixPlugin.autoImport._
