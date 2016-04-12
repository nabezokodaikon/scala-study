lazy val commonSettings = Seq(
  organization := "com.github.nabezokodaikon",
  version := "0.0.1",
  scalaVersion := "2.11.8",
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Xlint"
  ),
  shellPrompt := { state =>
    val branch = if (file(".git").exists) {
      "git branch".lines_!.find { _.head == '*' }.map { _.drop(1) }.getOrElse("")
    } else ""
    Project.extract(state).currentRef.project + branch + " > "
  }
)

lazy val root = (project.in(file(".")))
  .settings(commonSettings: _*)
  .settings(
    name := "scala-study",
    resolvers ++= {
      Seq(
      )
    },
    libraryDependencies ++= {
      Seq(
        // Test
        "org.scalactic" %% "scalactic" % "3.0.0-M15",
        "org.scalatest" %% "scalatest" % "3.0.0-M15" % "test",
        "org.scalacheck" %% "scalacheck" % "1.13.0" % "test",

        // Logger
        "ch.qos.logback" % "logback-classic" % "1.1.7",
        "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"
      )
    }
  )
