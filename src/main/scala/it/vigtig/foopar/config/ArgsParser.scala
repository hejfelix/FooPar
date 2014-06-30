package it.vigtig.foopar.config

trait ArgsParser {

  implicit class Parsable(args: Array[String]) {
    def getValue(dashName: String): String =
      if (!args.contains(dashName))
        sys.error("No argument: " + dashName + " in args-list")
      else
        args(args.indexOf(dashName) + 1)
    def getValueOr(dashName: String, v: String): String =
      if (!args.contains(dashName))
        v
      else
        args(args.indexOf(dashName) + 1)
  }

}