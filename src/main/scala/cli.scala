import java.lang.Integer

import scala.util.parsing.combinator.RegexParsers
import scala.io.Source

object defaults {
//  val filename = "sample_data/#iphonedev_20120611.log-short"
  val filename = "sample_data/#iphonedev_20120611.log"
}

sealed trait IRCLine {
  val timestamp: Integer
  val nick: String

}
case class Message(timestamp: Integer, nick: String, message: String) extends IRCLine {
  val _targets = LineParser.extractTargets(message)
}
case class Join(timestamp: Integer, nick: String, host: String) extends IRCLine
case class Part(timestamp: Integer, nick: String, host: String, message: String) extends IRCLine
case class Quit(timestamp: Integer, nick: String, host: String, reason: String) extends IRCLine
case class NickChange(timestamp: Integer, oldnick: String, nick: String) extends IRCLine
case class Action(timestamp: Integer, nick: String, action: String) extends IRCLine
case class ModeChange(timestamp: Integer, nick: String, mode: String, target: String) extends IRCLine

object LineParser extends RegexParsers {
  override val skipWhitespace = false

  val timestamp_number: Parser[Integer] = """\d+""".r ^^ { _.toInt }
  val timestamp = ( "[" ~> timestamp_number ) ~ ( ":" ~> timestamp_number ) ~ ( ":" ~> timestamp_number <~ "]" ) ^^ {
    case h ~ m ~ s => h * 60 * 60 + m * 60 + s
  }

  val mode = "[+-][a-zA-Z]+".r
  val channel = "[#&][^, ]{1,200}".r

  val nick_letter = "[a-zA-Z]+".r
  val nick_digit = "[0-9]+".r
  val nick_special = ( "[" | "]" | "\\" | "`" | "_" | "^" | "{" | "}" | "|" | "-" )
  val nick = (nick_letter | nick_special) ~ rep(nick_letter | nick_digit | nick_special) ^^ {
    case first ~ list => first ++ list.mkString
  }
  val nicks: Parser[List[String]] = rep( nick <~ "[,:] ?".r ) <~ rest
  val hostnick = rep( nick_letter | nick_digit | nick_special | "." ) ^^ {
    case list => list.mkString
  }

  val hostchars = "[:/a-zA-Z0-9._-]+".r
  val host = "~?".r ~ hostnick ~ "@" ~ hostchars ^^ { case _1 ~ n ~ a ~ _2 => _1 ++ n ++ a ++ _2 }
  val hostmask = rep( nick | "*" ) ~ "!" ~ rep( nick | "*" ) ~ "@" ~ rep( hostchars | "*" )  ^^ { case n1 ~ b ~ n2 ~ a ~ h => n1.mkString ++ b ++ n2.mkString ++ a ++ h.mkString }

  val paren_reason: Parser[String] = ( "\\(\"(.*)\"\\)".r | "\\((.*)\\)".r)

  val rest = ".*".r

// [00:01:58] <Gur_Gvpx> revpn: url
  val message = (timestamp <~ " ") ~ ( "<" ~> nick <~ "> " ) ~ rest ^^ { case ts ~ n ~ m => Message(ts, n, m) }

// [00:05:31] *** Quits: jryyl (~jryyl@hanssvyvngrq/jryyl) (Ping timeout: 252 seconds)
  val quits = (timestamp <~ " ") ~ ( "*** Quits: " ~> nick ) ~ (" (" ~> host <~ ") ") ~ paren_reason ^^ { case ts ~ n ~ h ~ r => Quit(ts, n, h, r) }

// [00:07:26] *** Joins: ecbjryy (~ecbjryy@PCR-58-168-95-254.yaf6.xra.ovtcbaq.arg.nh)
  val joins = (timestamp <~ " ") ~ ( "*** Joins: " ~> nick ) ~ (" (" ~> host <~ ")") ^^ { case ts ~ n ~ h => Join(ts, n, h) }

// [03:27:10] *** Parts: gurOynpx (~guroynpx@93-136-4-134.nqfy.arg.g-pbz.ue) ()
  val parts = (timestamp <~ " ") ~ ( "*** Parts: " ~> nick ) ~ (" (" ~> host <~ ")") ~ (" " ~> paren_reason) ^^ { case ts ~ n ~ h ~ r => Part(ts, n, h, r) }

// [00:21:05] *** qentbafu-1 is now known as qentbafurq
  val nickchange = (timestamp <~ " ") ~ ( "*** " ~> nick ) ~ (" is now known as " ~> nick) ^^ { case ts ~ oldn ~ newn => NickChange(ts, oldn, newn) }

// [01:09:42] * WbangunaGubzcfba jbaqref vs N_Aho erfcbaqrq ng rknpgyl gur evtug gvzr gb pngpu uvz abg pbaarpgrq :P
  val action = (timestamp <~ " ") ~ ( "* " ~> nick ) ~ rest ^^ { case ts ~ nick ~ rest => Action(ts, nick, rest) }

// [03:01:01] *** ChanServ sets mode: +o xEnXnGbN
// [03:01:37] *** xEnXnGbN sets mode: +b *!*RivyCrath@*.nyod.djrfg.arg
  val modetarget = ( nick | channel | hostmask )
  val modechange = (timestamp <~ " ") ~ ( "*** " ~> nick <~ " sets mode: ") ~ mode ~ ( " " ~> modetarget ) ^^ { case ts ~ nick ~ mode ~ target => ModeChange(ts, nick, mode, target) }


  val line = ( message | quits | joins | parts | nickchange | action | modechange )

  def apply(input: String) = parseAll(line, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }

  def extractTargets(input: String): List[String] = parseAll(nicks, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }
}

class IRCLog(rawlines: List[String]) {
  val lines: List[IRCLine] = for (line <- rawlines; parse = LineParser(line) ) yield parse
  val nicks = (for(line <- lines) yield line.nick).toList.distinct.sorted
//  val messages = lines.collect { case m@Message(_, _, _) => m } // @ and : are interchangeable here, @ allows the use of unpacked parameters to Message.
  val messages = lines.collect { case m:Message => m }

  val talkative = nicks.map(nick => (nick, messages.filter(message => message.nick == nick).toList.length)) toMap
  val mentioned = nicks.map(nick => (nick, messages.filter( _ match { case message: Message => message.message.contains(nick); case _ => false } ).toList.length)) toMap
  val targeted = nicks.map(nick => (nick, messages.filter( _ match { case m: Message => m._targets.contains(nick); case _ => false } ).toList.length ) )

  val most_talkative = talkative.map( kv => (kv._2, kv._1) ).toList.sorted.reverse
  val most_mentioned = mentioned.map( kv => (kv._2, kv._1) ).toList.sorted.reverse
}

object IRCLog {
  def fromFile(filename: String, encoding: String = "UTF-8") = {
    val source = Source.fromFile(filename, encoding)
    val lines = source.getLines.toList
    new IRCLog(lines)
  }
}

object ircstats {
  def main(args: Array[String]) {
    val s = """
[00:01:58] <Gur_Gvpx> revpn: url
[00:01:58] <Gur_Gvpx> revpn, jryyl: url
[00:05:31] *** Quits: jryyl (~jryyl@hanssvyvngrq/jryyl) (Ping timeout: 252 seconds)
[00:07:26] *** Joins: ecbjryy (~ecbjryy@PCR-58-168-95-254.yaf6.xra.ovtcbaq.arg.nh)
[03:27:10] *** Parts: gurOynpx (~guroynpx@93-136-4-134.nqfy.arg.g-pbz.ue) ()
[00:21:05] *** qentbafu-1 is now known as qentbafurq
[01:09:42] * WbangunaGubzcfba jbaqref vs N_Aho erfcbaqrq ng rknpgyl gur evtug gvzr gb pngpu uvz abg pbaarpgrq :P
[03:01:01] *** ChanServ sets mode: +o xEnXnGbN
[03:01:37] *** xEnXnGbN sets mode: +b *!*RivyCrath@*.nyod.djrfg.arg
""".trim
    val lines = s.split("\n").toList

    var log = if(args.length == 1)
      new IRCLog(lines)
    else
      IRCLog.fromFile(defaults.filename)

    val m = log.messages(0)
    val r = m.getTargets
    println(m)
    println(r)
  }
}
