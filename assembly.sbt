import sbtassembly.Plugin.AssemblyKeys._

assemblySettings

jarName in assembly := { s"${name.value}-${version.value}.jar" }