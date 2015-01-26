lazy val root = (project in file(".")).
  settings(
    name := "datastructures",
    version := "1.0",
    scalaVersion := "2.11.5",
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.1.0",
      "org.specs2" %% "specs2-core" % "2.4.15" % "test"
    ),
    scalacOptions ++= Seq("-Xfatal-warnings", "-feature", "-deprecation"),
    scalacOptions in Test ++= Seq("-Yrangepos")
  )