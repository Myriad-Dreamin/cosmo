package cosmo

// def println(x: Any): Unit = Console.println(x)

def debugln(f: => Any): Unit = {}
def logln(f: => Any): Unit = println(f)

val escapeStrPattern = "(\\r\\n|[\"\\\\\\t\\f\\r\\n])".r
val unescapeStrPattern = "(\\\\[\\\\tfrn\\\"])".r

def escapeStr(s: String): String = {
  escapeStrPattern.replaceAllIn(
    s,
    (m) => {
      m.group(1) match {
        case "\""   => "\\\\\""
        case "\\"   => "\\\\\\"
        case "\n"   => "\\\\n"
        case "\t"   => "\\\\t"
        case "\r"   => "\\\\r"
        case "\f"   => "\\\\f"
        case "\r\n" => "\\\\n"
        case other  => other
      }
    },
  )
}

def unescapeStr(s: String): String = {
  unescapeStrPattern.replaceAllIn(
    s,
    (m) => {
      m.group(1) match {
        case "\\\"" => "\""
        case "\\\\" => "\\"
        case "\\n"  => "\n"
        case "\\t"  => "\t"
        case "\\r"  => "\r"
        case "\\f"  => "\f"
        case other  => other
      }
    },
  )
}
