import BuildHelper._

addCommandAlias("build", "prepare; test")
addCommandAlias("prepare", "fix; fmt")
addCommandAlias("check", "fixCheck; fmtCheck")
addCommandAlias("fix", "all compile:scalafix test:scalafix")
addCommandAlias("fixCheck", "compile:scalafix --check; test:scalafix --check")
addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("fmtCheck", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")

inThisBuild(
  List(
    organization      := "com.schuwalow",
    homepage          := Some(url("https://github.com/mschuwalow/play-ziojson")),
    developers        := List(
      Developer(
        "mschuwalow",
        "Maxim Schuwalow",
        "maxim.schuwalow@gmail.com",
        url("https://github.com/mschuwalow")
      )
    ),
    licenses          := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalaVersion      := Scala213
  )
)

lazy val root = (project in file("."))
  .settings(
    publish / skip     := true,
    crossScalaVersions := Nil
  )
  .aggregate(lib, example)

lazy val lib = (project in file("lib"))
  .settings(extraCompilerOptions)
  .settings(
    name               := "play-ziojson",
    testFrameworks     := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Dependencies.Lib,
    crossScalaVersions := Seq(Scala213, Scala3)
  )

lazy val example = (project in file("example"))
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLayoutPlugin)
  .settings(publish / skip := true)
  .dependsOn(lib)
  .settings(extraCompilerOptions)
  .settings(
    name           := "play-ziojson-example",
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies += guice
  )
