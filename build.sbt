name := "scalalgebra"

version := "0.1"

scalaVersion := "2.13.8"
scalacOptions += "-Xlog-implicits"
scalacOptions += "-Vimplicits-verbose-tree"
scalacOptions += "-Vimplicits"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.12"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % "test"
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.9"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.0"