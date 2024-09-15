package cosmo.formatter

import scala.scalajs.js

import cosmo._

trait FormatProvider {
  def formatCode(code: String): String
}

enum FormatterKind {
  case ClangFormat
  case Unknown
}

var formatter: Option[FormatProvider] = None
var formatterChecked: Boolean = false

def checkFormatter() = {
  formatterChecked = true
  var formatterPath: Option[String] = None

  if (NodeFs.existsSync("config.json")) {
    val config = NodeFs.readFileSync("config.json", "utf8").asInstanceOf[String]
    val configJson = scalajs.js.JSON.parse(config)
    configJson.formatter.asInstanceOf[js.UndefOr[String]].toOption match {
      case Some(path) => formatterPath = Some(path)
      case None       =>
    }
  }

  formatterPath match {
    case Some(path) =>
      // check the formatter kind
      val result = NodeChildProcess.spawnSync(
        path,
        js.Array("--version"),
        js.Dynamic.literal(
          encoding = "utf8",
          stdio = "pipe",
        ),
      )

      result.status.toOption match {
        case Some(0) => {
          var kind = FormatterKind.Unknown
          if (result.stdout.asInstanceOf[String].contains("clang-format")) {
            kind = FormatterKind.ClangFormat
          }

          if (kind == FormatterKind.Unknown) {
            println(s"Unknown formatter $path, use it anyway")
          }

          formatter = Some(new FormatProvider {
            def formatCode(code: String): String = {
              val result = NodeChildProcess.spawnSync(
                path,
                js.Array(),
                js.Dynamic.literal(
                  input = code,
                  encoding = "utf8",
                  stdio = "pipe",
                ),
              )
              result.stdout.asInstanceOf[String]
            }
          })
        }
        case _ => {
          println(s"Formatter $path is not working, will not use it")
        }
      }
    case None =>
  }
}

def formatCode(code: String): String = {
  if (!formatterChecked) {
    checkFormatter()
  }

  formatter match {
    case Some(f) => f.formatCode(code)
    case None =>
      code
  }
}
