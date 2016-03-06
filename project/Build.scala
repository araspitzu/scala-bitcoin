import sbt.Keys._
import sbt._

object SomeBuild extends Build {

  import BuildSettings._
  import Dependencies._
  import Resolvers._

  val name = "core"
  lazy val scalaBitcoin = Project(
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

object BuildSettings {
  val buildOrganization = "scala-bitcoin"
  val buildVersion = "0.0.1"
  val buildScalaVersion = "2.11.7"
  val buildSbtVersion = "0.13.8"

  val buildSettings = Defaults.coreDefaultSettings ++ Seq(
    organization := buildOrganization,
    sbtVersion := buildSbtVersion,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-encoding", "utf8"),
    javaOptions += "-Xmx1G",
    shellPrompt := ShellPrompt.buildShellPrompt
  )
}

object Resolvers {
  val typesafe = "Typesafe Releases" at "https://repo.typesafe.com/typesafe/releases"
  val akka = "Akka Releases" at "http://repo.akka.io/releases/"
  val mavenIvy = Resolver.url("Maven public", url("https://repo1.maven.org/maven2"))(Resolver.ivyStylePatterns)
  val repositories = Seq(typesafe, akka, mavenIvy)
}

object Dependencies {
  val akkaVersion = "2.3.9"

  val crypto = Seq(
    "org.bouncycastle" % "bcprov-jdk16" % "1.45"
  )

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

  val dependencies = crypto ++ akka ++ logging ++ testing
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

  val buildShellPrompt = (state: State) => {
      val currProject = Project.extract(state).currentProject.id
      "%s:%s:%s> ".format(
        currProject, currBranch, BuildSettings.buildVersion
      )
  }

}