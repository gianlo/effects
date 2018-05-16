name := "effects"

version := "0.1"

scalaVersion := "2.12.6"

scalacOptions ++= Seq(
  "-Ypartial-unification",
  "-feature")
resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")