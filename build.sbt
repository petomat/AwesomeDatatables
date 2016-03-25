name := "AwesomeDatatables"

version := "1.0"

scalaVersion := "2.11.8"

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)


libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.0",
  "org.specs2" %% "specs2-core" % "2.4.17"
)


