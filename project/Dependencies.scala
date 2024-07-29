import sbt._

object Dependencies {

  object Versions {
    val zio = "2.1.6"
  }

  import Versions._

  val Core = Seq(
    "dev.zio" %% "zio"          % zio,
    "dev.zio" %% "zio-test"     % zio,
    "dev.zio" %% "zio-test-sbt" % zio % Test
  )
}
