package cosmo

import scala.annotation.switch

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

object Chars:

  inline val LF = '\u000A'
  inline val FF = '\u000C'
  inline val CR = '\u000D'
  inline val SU = '\u001A'

  /** Is character a line break? */
  def isLineBreakChar(c: Char): Boolean = (c: @switch) match {
    case LF | FF | CR | SU => true
    case _                 => false
  }
end Chars

// https://github.com/scala/scala3/blob/eb5c3e81304eee5ce75c2796bd4b7b16d2d1b19b/compiler/src/dotty/tools/dotc/util/Util.scala
/** The index `i` in `candidates.indices` such that `candidates(i) <= x` and
  * `candidates(i)` is closest to `x`, determined by binary search, or -1 if `x
  * < candidates(0)`.
  * @param hint
  *   If between 0 and `candidates.length` use this as the first search point,
  *   otherwise use `candidates.length/2`.
  * @pre
  *   candidates is sorted
  */
def bestFit(
    candidates: Array[Int],
    length: Int,
    x: Int,
    hint: Int = -1,
): Int = {
  def recur(lo: Int, hi: Int, mid: Int): Int =
    if (x < candidates(mid))
      recur(lo, mid - 1, (lo + mid - 1) / 2)
    else if (mid + 1 < length && x >= candidates(mid + 1))
      recur(mid + 1, hi, (mid + 1 + hi) / 2)
    else mid
  val initMid = if (0 <= hint && hint < length) hint else length / 2
  if (length == 0 || x < candidates(0)) -1
  else recur(0, length, initMid)
}
