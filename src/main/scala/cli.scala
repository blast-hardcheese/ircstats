import scala.io.Source

object defaults {
  def filename = "sample_data/#iphonedev_20120611.log-short"
}

object ircstats {
  def main(args: Array[String]) {
    val s = Source.fromFile(defaults.filename)
    for(line <- s.getLines) {
      println(line)
    }
  }
}
