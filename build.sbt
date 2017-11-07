val nexus = "https://oss.sonatype.org/"
val nexusSnapshots = nexus + "content/repositories/snapshots";
val nexusReleases = nexus + "service/local/staging/deploy/maven2";

organization := "com.mchange"

name := "yinyang"

version := "0.0.2"

scalaVersion := "2.11.8"

crossScalaVersions := Seq("2.10.6", scalaVersion.value, "2.12.4")

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

resolvers += ("releases" at nexusReleases)

resolvers += ("snapshots" at nexusSnapshots)

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.5" % "test"

publishTo := {
  if (version.value.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexusSnapshots )
  else
    Some("releases"  at nexusReleases )
}

pomExtra := {
  <url>https://github.com/swaldman/{name.value}</url>
  <licenses>
    <license>
      <name>GNU Lesser General Public License, Version 2.1</name>
      <url>http://www.gnu.org/licenses/lgpl-2.1.html</url>
      <distribution>repo</distribution>
    </license>
    <license>
      <name>Eclipse Public License, Version 1.0</name>
      <url>http://www.eclipse.org/org/documents/epl-v10.html</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:swaldman/{name.value}.git</url>
    <connection>scm:git:git@github.com:swaldman/{name.value}</connection>
  </scm>
  <developers>
    <developer>
      <id>swaldman</id>
      <name>Steve Waldman</name>
      <email>swaldman@mchange.com</email>
    </developer>
  </developers>
}



