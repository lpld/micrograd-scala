import sbt._

object Dependencies {
  object org {
    object graphstream {

      val version = "2.0"
      val `gs-core` =
        "org.graphstream" % "gs-core" % version

      val `gs-algo` =
        "org.graphstream" % "gs-algo" % version

      val `gs-ui-swing` =
        "org.graphstream" % "gs-ui-swing" % version

      val `gs-ui-javafx` =
        "org.graphstream" % "gs-ui-javafx" % version
    }
  }
}
