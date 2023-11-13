package prg1.lx09.regexp

import RegularExpressionVM._

class VMTest(vm: VM, vmname: String, shortName: String) {

  object TooSlow extends Exception

  val a = C('a')
  val optionallyA = Alternate(a, Empty)

  def benchmark(timeout: Double): Unit = {

    def benchmark(k: Int): Unit = {
      def ak(k: Int): RegularExpression = {
        if (k == 0) Empty
        else Concatenate(optionallyA, Concatenate(ak(k-1), a))
      }
      val rx = ak(k)
      val program = rx.compile
      val s = "a" * k
      println(f"$shortName%8s: 'a^$k' matches '(a?)^${k}a^$k'")
      vm.execute(program, s)
    }

    try {
      for (k <- 24 to 30) {
        val t_start = System.nanoTime()
        benchmark(k)
        val t = (System.nanoTime() - t_start) / 1000000000.0
        println(f"$k%8d: $t%5.2fs")
        if (t > timeout) throw TooSlow
      }
    } catch { case TooSlow => println("時間がかかりすぎるので、ここで打ち止め\n ") }
  }

  def run(): Unit = {
    println(f"Benchmarking $vmname")
    benchmark(5)
  }
}

@main def test : Unit = {
  new VMTest(RecursiveBacktrackingVM,  "Recursive backtracking virtual machine", "REC")     .run()
  new VMTest(IterativeBacktrackingVM,  "Iterative backtracking virtual machine", "ITER1")   .run()
  new VMTest(IterativeBacktrackingVM2, "Iterative backtracking virtual machine", "ITER2")   .run()
  new VMTest(KenThompsonVM,            "Ken Thompson's virtual machine",         "Thompson").run()
}
