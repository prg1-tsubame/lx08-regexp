package prg1.lx09.regexp

import scala.util.matching.Regex
import scala.util.matching.Regex._

object Example {
  def main(arguments: Array[String]): Unit = {

    def printMatchResult(result: Option[String]): Unit = {
      println(result match {
        case Some(s) => s case _ => "適合しませんでした"
      })
    }

    def printAllMatches(result: MatchIterator): Unit = {
      result.foreach(println)
      println()
    }

    // 文字列の後ろに .r があることに注意
    // 文字列として与えられた正規表現をコンパイルする。
    // 見掛け上は scala.util.matching.Regex というクラスだが、
    // 正規表現とのマッチングは java.util.regex.Pattern に丸投げしている
    val p1: Regex = "aa*".r

    // findPrefixOf: CharSequence => Option[String]
    // 文字列の先頭から正規表現に合致した部分を返す
    println("findPrefixOfの実験")
    printMatchResult(p1.findPrefixOf("a"))             // a
    printMatchResult(p1.findPrefixOf("aaa"))           // aaa
    printMatchResult(p1.findPrefixOf("   aaa aaaaa"))  // 適合しません

    // findFirstIn: CharSequence => Option[String]
    // 文字列のなかで正規表現に最初に合致した部分を返す
    println("\nfindFirstInの実験")
    printMatchResult(p1.findFirstIn("a"))              // a
    printMatchResult(p1.findFirstIn("aaa"))            // aaa
    printMatchResult(p1.findFirstIn("   aaa aaaaa"))   // aaa

    // findAllIn: CharSequence => MatchIterator
    println("\nfindAllInの実験")
    printAllMatches(p1.findAllIn("a"))
    printAllMatches(p1.findAllIn("aaa"))            // aaa
    printAllMatches(p1.findAllIn("   aaa aaaaa"))   // aaa, aaaaa

    // replaceFirstIn: (CharSequence, String) => String
    println("replaceFirstInの実験")
    println(p1.replaceFirstIn("a", "X"))            // X
    println(p1.replaceFirstIn("aaa", "X"))          // X
    println(p1.replaceFirstIn("   aaa aaaaa", "X")) //    X aaaaa

    // replaceAllIn: (CharSequence, String) => String
    println("\nreplaceAllInの実験")
    println(p1.replaceAllIn("a", "X"))            // X
    println(p1.replaceAllIn("aaa", "X"))          // X
    println(p1.replaceAllIn("   aaa aaaaa", "X")) //    X X

    println(p1.replaceAllIn(
      "   aaa aaaa aaaaa",
      (m: Match) => "[" + (if (m.matched.length % 2 == 0) "偶数"
                           else "奇数") + "個のa]" ))
    //    [奇数個のa] [偶数個のa] [奇数個のa]

    // replaceSomeIn: (CharSequence, (Match) => Option[String]) => String
    println("\nreplaceSomeInの実験")
    println(p1.replaceSomeIn(
      "   aaa aaaa aaaaa",
      (m: Match) =>
        if (m.matched.length % 2 == 0) Some("[偶数個のa]")
        else None))
    //    aaa [偶数個のa] aaaaa

    println("\nfindFirstMatchInの実験1")
    def d(n: Int): String = "[0-9]" * n
    val datetime_r1 = {
      val d2 = d(2)
      val d4 = d(4)
      val ymd = f"($d4)/($d2)/($d2)"
      val hm = f"($d2):($d2)"
      val datetime = ymd + "  *" + hm
      println(f" - $datetime")
      datetime.r
    }

    // findFirstMatchIn: CharSequence => Option[Match]
    val date_string = "The time of this writing is: 2022/11/14 13:50"

    datetime_r1.findFirstMatchIn(date_string) match {
      case Some(m) => {
        println(f" - 適合した部分: ${m.group(0)}")
        println(f" - Year: ${m.group(1)}")
        println(f" - Month: ${m.group(2)}")
        println(f" - Date: ${m.group(3)}")
        println(f" - Hour: ${m.group(4)}")
        println(f" - Minute: ${m.group(5)}")
      }
      case _ => ()
    }

    println("\nfindFirstMatchInの実験2")
    val datetime_r2 = {
      val d2 = d(2)
      val d4 = d(4)
      val ymd = f"($d4)/($d2)/($d2)"
      val hm = f"($d2):($d2)"
      val datetime = ymd + "  *" + hm
      println(f" - $datetime")
      new Regex(datetime, "Y", "M", "D", "H", "min")
    }

    datetime_r2.findFirstMatchIn(date_string) match {
      case Some(m) => {
        println(f"Hour:   ${m.group("H")}")
        println(f"minute: ${m.group("min")}")
        println()
        "Y,M,D".split(",").foreach((name: String) => println((name, m.group(name))))
      }
      case None => ()
    }
  }
}
