name := "NeatDatatables"

version := "1.0"

scalaVersion := "2.11.8"

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)


libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.0",
  "org.specs2" %% "specs2-core" % "3.7.2"
  // current release of specs2 does not print datatables in the success case but the latest version will do with some
  // indentation und printing issues, see: https://gitter.im/etorreborre/specs2?at=56fce50ee4a8384a1bbd8765
  // "org.specs2" %% "specs2-core" % "3.7.2-20160331090154-e81d66a"
)


