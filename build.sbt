ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.4.2"
val _scalacOptions = Seq(
  "-deprecation",
  "-feature",
  "-Xfatal-warnings",
  "-unchecked",
  "-no-indent",
  "-old-syntax",
  "-explain"
)

lazy val root = (project in file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name                            := "laminar_test",
    libraryDependencies ++= Seq(
      "com.raquo" %%% "laminar" % "17.0.0"
    ),
    scalaJSUseMainModuleInitializer := true,
    jsEnv                           := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
  )
