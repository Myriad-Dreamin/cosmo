package cosmo

// def println(x: Any): Unit = Console.println(x)

def debugln(f: => Any): Unit = {}
def logln(f: => Any): Unit = println(f)

private val escapeStrPattern = "((?:\\r\\n)|[\"\\\\\\t\\f\\r\\n])".r
private val unescapeStrPattern = "(\\\\[\\\\tfrn\\\"])".r

def canoPath(lhs: String): String =
  NodePath.normalize(lhs).replace('\\', '/') match
    case "." => ""; case x => x

def libPath(lhs: String): syntax.Node = {
  val idents = lhs.split('.').map(syntax.Ident.apply);
  idents.reduceLeft(syntax.Select(_, _, true))
}

def escapeStr(s: String): String = {
  escapeStrPattern.replaceAllIn(
    s,
    (m) => {
      m.group(1) match {
        case "\""   => "\\\\\""
        case "\\"   => "\\\\\\\\"
        case "\n"   => "\\\\n"
        case "\t"   => "\\\\t"
        case "\r"   => "\\\\r"
        case "\f"   => "\\\\f"
        case "\r\n" => "\\\\n"
        case other  => "\\" + other
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

def bytesRepr(s: Array[Byte]): String = {
  var sb = new StringBuilder()
  // \xbb
  for (b <- s) {
    sb.append(f"\\x${b}%02x")
  }

  sb.toString()
}
