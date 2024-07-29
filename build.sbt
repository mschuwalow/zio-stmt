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
    homepage          := Some(url("https://github.com/mschuwalow/zio-stmt")),
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
  .aggregate(core)

lazy val core = (project in file("core"))
  .settings(extraCompilerOptions)
  .settings(
    name               := "zio-stmt",
    testFrameworks     := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Dependencies.Core,
    crossScalaVersions := Seq(Scala213, Scala3)
  )
