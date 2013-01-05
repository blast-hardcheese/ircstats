import java.lang.Integer

import scala.util.parsing.combinator.RegexParsers
import scala.io.Source

object defaults {
  def filename = "sample_data/#iphonedev_20120611.log-short"
}

object LineParser extends RegexParsers {
  def timestamp_number: Parser[Integer] = """\d+""".r ^^ { _.toInt }
  def timestamp = "[" ~> timestamp_number ~ ":" ~ timestamp_number ~ ":" ~ timestamp_number <~ "]" ^^ {
    case h ~ ":" ~ m ~ ":" ~ s => h * 60 * 60 + m * 60 + s
  }

  val nick_letter: Parser[String] = "[a-zA-Z]+".r
  val nick_digit: Parser[String] = "[0-9]+".r
  val nick_special: Parser[String] = ( "[" | "]" | "\\" | "`" | "_" | "^" | "{" | "}" | "|" )
  def user: Parser[String] = "<" ~ (nick_letter | nick_special) ~ rep(nick_letter | nick_digit | nick_special) ~ ">" ^^ {
    case first ~ list => list.mkString
  }
  val rest = ".*".r
  def line = timestamp ~ rest ^^ {
    case ts ~ r => Tuple2(ts, r)
  }

  def apply(input: String) = parseAll(line, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }
}

object ircstats {
  def main(args: Array[String]) {
    val s = Source.fromFile(defaults.filename)
    for(line <- s.getLines) {
      println(line)
      println(LineParser.apply(line))
    }
  }
}
