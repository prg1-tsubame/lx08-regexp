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
    Scala Build Tool (sbt) 1.11.6 (Eclipse Adoptium Java 17.0.12)
    Scala Version: 3.7.2

    MacBook Air (Apple M3, 2024 model, Apple M3, 16GB)

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
17:  0.00s
18:  0.00s
19:  0.01s
20:  0.01s
21:  0.02s
22:  0.04s
23:  0.08s
24:  0.15s
25:  0.32s
26:  0.63s
27:  1.30s
28:  2.59s
29:  5.34s
30: 10.69s
**/