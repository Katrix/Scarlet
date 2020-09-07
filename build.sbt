lazy val scarlet = project.settings(
  name := "Scarlet",
  organization := "net.katsstuff",
  version := "0.1",
  scalaVersion := "2.13.3",
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),
  libraryDependencies += "org.scodec"        %% "scodec-core"  % "1.11.7",
  libraryDependencies += "com.lihaoyi"       %% "pprint"       % "0.6.0",
  libraryDependencies += "com.beachape"      %% "enumeratum"   % "1.6.1",
  libraryDependencies += "com.lihaoyi"       %% "fastparse"    % "2.3.0",
  libraryDependencies += "org.apache.commons" % "commons-text" % "1.9",
  libraryDependencies ++= Seq(
    "org.scalameta" %% "scalameta"        % "4.3.21",
    "org.scalameta" %% "scalafmt-dynamic" % "2.6.4"
  ),
  libraryDependencies += "org.typelevel"    %% "cats-core"  % "2.2.0",
  libraryDependencies += "com.chuusai"      %% "shapeless"  % "2.3.3",
  libraryDependencies += "org.scala-graph"  %% "graph-core" % "1.13.2",
  libraryDependencies += "com.github.scopt" %% "scopt"      % "4.0.0-RC2",
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Xlint",
    "-Ywarn-dead-code"
  )
)

lazy val scarletAll = project.in(file(".")).aggregate(scarlet)
