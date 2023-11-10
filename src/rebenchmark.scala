package prg1.lx09.regexp

/**
 * Scala の正規表現機能についての実験
 * a?a?..a?aa..a のような正規表現の aaa のような文字列への適用に要する時間の測定
 **/

object Benchmark {
  def benchmark(pattern: scala.util.matching.Regex, string: String): Double = {
      val time_start = System.nanoTime()
      pattern.findFirstIn(string)
      (System.nanoTime() - time_start) / 1000000000.0
  }

  @main def findFirstBenchmark() = {
    var i = 1
    var cont = true
    while (cont) {
      val t = benchmark(("a?" * i + "a" * i).r, "a" * i)
      println(f"$i%2d: $t%5.2fs")
      if (t > 10) cont = false
      i = i + 1
    }
  }
}

/**
ベンチマークの実行結果

実行環境
    Scala Build Tool (sbt) 1.7.1 (Azul Systems, Inc. Java 19.0.2)
    Scala Version: 3.2.2
    Java Development Kit: zulu19.32.15-ca-fx-jdk19.0.2-macosx_aarch64

    MacBook Air (M1, 2020 model, Apple M1, 16GB 1600 MHz DDR3)

 1:  0.00s
 2:  0.00s
 3:  0.00s
 4:  0.00s
 5:  0.00s
 6:  0.00s
 7:  0.00s
 8:  0.00s
 9:  0.00s
10:  0.00s
11:  0.00s
12:  0.00s
13:  0.00s
14:  0.00s
15:  0.00s
16:  0.00s
17:  0.01s
18:  0.03s
19:  0.01s
20:  0.02s
21:  0.03s
22:  0.06s
23:  0.11s
24:  0.23s
25:  0.46s
26:  0.95s
27:  1.86s
28:  3.85s
29:  7.59s
30: 15.73s
**/