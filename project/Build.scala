import sbt.Keys._
import sbt._

object BuildSettings {
  val buildOrganization = "scala-bitcoin"
  val buildVersion = "0.0.1"
  val buildScalaVersion = "2.11.6"

  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-encoding", "utf8"),
    javaOptions += "-Xmx1G",
    shellPrompt := ShellPrompt.buildShellPrompt
  )
}

object Resolvers {
  val typesafe = "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases"
  val akka = "Akka Releases" at "http://repo.akka.io/releases/"
  val repositories = Seq(typesafe, akka)
}


object Dependencies {
  val akkaVersion = "2.3.9"

  val akka = Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "com.typesafe.akka" %% "akka-slf4j" % akkaVersion
  )

  val logging = Seq(
    "ch.qos.logback" % "logback-classic" % "1.1.2",
    "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2"
  )

  val testing = Seq(
    "org.specs2" %% "specs2" % "2.3.12" % "test",
    "org.specs2" %% "specs2-mock" % "2.3.12" % "test",
    "org.specs2" %% "specs2-matcher-extra" % "2.3.12" % "test",
    "org.mockito" % "mockito-core" % "1.9.5" % "test"
  )

  val dependencies = akka ++ logging ++ testing
}

object ThisBuild extends Build {

  import BuildSettings._
  import Dependencies._
  import Resolvers._

  val name = "core"
  lazy val api = Project(
    name, file("."),
    settings = buildSettings ++ Seq(
      resolvers := repositories,
      libraryDependencies ++= dependencies
    )
  )

  def currentGitBranch = {
    "git rev-parse --abbrev-ref HEAD".lines_!.mkString.replaceAll("/", "-").replaceAll("heads-", "")
  }

}

// Shell prompt which show the current project,
// git branch and build version
object ShellPrompt {

  object devnull extends ProcessLogger {
    def info(s: => String) {}

    def error(s: => String) {}

    def buffer[T](f: => T): T = f
  }

  def currBranch = (
    ("git status -sb" lines_! devnull headOption)
      getOrElse "-" stripPrefix "## "
    )

  val buildShellPrompt = {
    (state: State) => {
      val currProject = Project.extract(state).currentProject.id
      "%s:%s:%s> ".format(
        currProject, currBranch, BuildSettings.buildVersion
      )
    }
  }
}