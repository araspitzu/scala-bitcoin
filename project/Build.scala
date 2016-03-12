import sbt.Keys._
import sbt._

object RootBuild extends Build {

  import BuildSettings._
  import Resolvers._
  import Dependencies._

  lazy val core = (project in file("core")).
    settings(buildSettings ++ Seq(
      resolvers := repositories,
      libraryDependencies ++= dependencies
    ))


  val name = "scala-bitcoin"
  lazy val scalaBitcoin = Project(
    name, file("."),
    settings = buildSettings
  ).dependsOn(core)


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
