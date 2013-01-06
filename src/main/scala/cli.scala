import java.lang.Integer

import scala.util.parsing.combinator.RegexParsers
import scala.io.Source

object defaults {
//  def filename = "sample_data/#iphonedev_20120611.log-short"
  def filename = "sample_data/#iphonedev_20120611.log"
}

sealed trait IRCLine
case class Message(timestamp: Integer, nick: String, message: String) extends IRCLine
case class Join(timestamp: Integer, nick: String, host: String) extends IRCLine
case class Part(timestamp: Integer, nick: String, host: String, message: String) extends IRCLine
case class Quit(timestamp: Integer, nick: String, host: String, reason: String) extends IRCLine
case class NickChange(timestamp: Integer, oldnick: String, newnick: String) extends IRCLine
case class Action(timestamp: Integer, nick: String, action: String) extends IRCLine
case class ModeChange(timestamp: Integer, nick: String, mode: String, target: String) extends IRCLine

object LineParser extends RegexParsers {
  override val skipWhitespace = false

  def timestamp_number: Parser[Integer] = """\d+""".r ^^ { _.toInt }
  def timestamp = "[" ~> timestamp_number ~ ":" ~ timestamp_number ~ ":" ~ timestamp_number <~ "]" ^^ {
    case h ~ ":" ~ m ~ ":" ~ s => h * 60 * 60 + m * 60 + s
  }

  val nick_letter = "[a-zA-Z]+".r
  val nick_digit = "[0-9]+".r
  val nick_special = ( "[" | "]" | "\\" | "`" | "_" | "^" | "{" | "}" | "|" | "-" | "." )
  val nick = (nick_letter | nick_special) ~ rep(nick_letter | nick_digit | nick_special) ^^ {
    case first ~ list => first ++ list.mkString
  }

  val hostnick = rep( nick_letter | nick_digit | nick_special ) ^^ {
    case list => list.mkString
  }

  val rest = ".*".r
  val message = "<" ~ nick ~ "> " ~ rest ^^ {
    case "<" ~ n ~ "> " ~ m => new Message(0, n, m)
  }

  val hostchars = "[:/a-zA-Z0-9._-]+".r
  val host = "~?".r ~ hostnick ~ "@" ~ hostchars ^^ { case _1 ~ n ~ a ~ _2 => _1 ++ n ++ a ++ _2 }
  val hostmask = rep( nick | "*" ) ~ "!" ~ rep( nick | "*" ) ~ "@" ~ rep( hostchars | "*" )  ^^ { case n1 ~ b ~ n2 ~ a ~ h => n1.mkString ++ b ++ n2.mkString ++ a ++ h.mkString }
// [03:01:37] *** kRaKaToA sets mode: +b *!*EvilPengu@*.albq.qwest.net
  val reason_paren: Parser[String] = "(" ~ reason ~ ")" ^^ {
    case _1 ~ _2 ~ _3 => _1 ++ _2 ++ _3
  }
  val reason: Parser[String] = ( "[^()]*".r | reason_paren | ")" )
  val quits = "*** Quits: " ~> nick ~ (" (" ~> host <~ ") ") ~ ("(" ~> reason <~ ")") ^^ { case n ~ h ~ r => new Quit(0, n, h, r) }
  val joins = "*** Joins: " ~> nick ~ (" (" ~> host <~ ")") ^^ { case n ~ h => new Join(0, n, h) }
  val parts = "*** Parts: " ~> nick ~ (" (" ~> host <~ ")") ~ (" (" ~> reason <~ ")") ^^ { case n ~ h ~ r => new Part(0, n, h, r) }
  val nickchange = "*** " ~> nick ~ (" is now known as " ~> nick) ^^ { case oldn ~ newn => new NickChange(0, oldn, newn) }
  val action = "* " ~> nick ~ rest ^^ { case nick ~ rest => new Action(0, nick, rest) }
  val mode = "[+-][a-zA-Z]+".r
  val channel = "[#&][^, ]{1,200}".r
  val modechange = "*** " ~> (nick <~ " sets mode: ") ~ mode ~ ( " " ~> ( nick | channel | hostmask ) ) ^^ { case nick ~ mode ~ target => new ModeChange(0, nick, mode, target) }


  def line = (timestamp <~ " ") ~ ( message | quits | joins | parts | nickchange | action | modechange ) ^^ {
    case ts ~ Message(_, n, m) => Message(ts, n, m)
    case ts ~ Quit(_, n, h, r) => Quit(ts, n, h, r)
    case ts ~ Join(_, n, h) => Join(ts, n, h)
    case ts ~ Part(_, n, h, r) => Part(ts, n, h, r)
    case ts ~ NickChange(_, o, n) => NickChange(ts, o, n)
    case ts ~ Action(_, n, a) => Action(ts, n, a)
    case ts ~ ModeChange(_, n, m, t) => ModeChange(ts, n, m, t)
  }

  def apply(input: String) = parseAll(line, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }

  def applyName(input: String) = parse(nick, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }
}

object ircstats {
  def main(args: Array[String]) {
    if(args.length == 1) {
      println(LineParser.mode findAllIn "+o" toList)
      println(LineParser.applyName("_bart"))
      val s1 = "[00:21:05] *** dragonsh-1 is now known as dragonshed"
      println(LineParser.apply(s1))

      val s2 = "[01:55:48] *** Joins: _bart (528b4fd4@gateway/web/freenode/ip.82.139.79.212)"
      println(LineParser.apply(s2))

      val s3 = "[03:01:01] *** ChanServ sets mode: +o kRaKaToA"
      println(LineParser.apply(s3))

      val s4 = "[03:01:37] *** kRaKaToA sets mode: +b *!*EvilPengu@*.albq.qwest.net"
      println(LineParser.apply(s4))

    } else {
      val s = Source.fromFile(defaults.filename, "UTF-8")
      for(line <- s.getLines) {
        println(line)
        println(LineParser.apply(line))
      }
    }
  }
}
