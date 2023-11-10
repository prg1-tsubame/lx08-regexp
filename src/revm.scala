package prg1.lx09.regexp

object RegularExpressionVM {

  trait Instruction
  case class Character(c: Char) extends Instruction
  case class Jump(x: Int) extends Instruction
  case class Split(x: Int, y: Int) extends Instruction
  case object Match extends Instruction

  type Program = Array[Instruction]
  type LProgram = List[Instruction]

  def printProgram(program: Program): Unit = {
    var pc = 0
    for (instruction <- program) {
      println(f"$pc%6s: $instruction")
      pc = pc + 1
    }
  }

  trait RegularExpression {
    def _compile(label: Int): (Int, LProgram)

    def compile: Program = (_compile(0)._2 ++ List(Match)).toArray
  }

  case object Empty extends RegularExpression {
    override def toString: String = ""

    def _compile(label0: Int): (Int, LProgram) = (label0, List())
  }

  case class C(c: Char) extends RegularExpression {
    override def toString: String = c.toString

    def _compile(label0: Int): (Int, LProgram) = {
      /*
       * L0: Character(c)
       * L1:
       */
      val (label1, char) = (label0+1, List(Character(c)))
      (label1, char)
    }
  }

  case class Concatenate(r1: RegularExpression, r2: RegularExpression) extends RegularExpression {
    override def toString: String = "(" + r1.toString + r2.toString + ")"

    def _compile(label0: Int): (Int, LProgram) = {
      /*
       * L0: R1を受理する命令列
       * L1: R2を受理する命令列
       * L2:
       */
      val (label1, program1) = r1._compile(label0)
      val (label2, program2) = r2._compile(label1)
      (label2, program1 ++ program2)
    }
  }

  case class Alternate(r1: RegularExpression, r2: RegularExpression) extends RegularExpression {
    override def toString: String = "(" + r1.toString + "|" + r2.toString + ")"

    def _compile(label0: Int): (Int, LProgram) = {
      /*
       * L0: Split(L1, L3)
       * L1: R1を受理する命令列
       * L2: Jump(L4)
       * L3: R2を受理する命令列
       * L4:
       */
      val label1 = label0 + 1 // Split(L1, L3)
      val (label2, program1) = r1._compile(label1)
      val label3 = label2 + 1 // Jump(L4)
      val (label4, program2) = r2._compile(label3)

      val split = List(Split(label1, label3))
      val jump = List(Jump(label4))

      (label4, split ++ program1 ++ jump ++ program2)
    }
  }

  case class Star(r: RegularExpression) extends RegularExpression {
    override def toString: String = "(" + r.toString + "*)"

    def _compile(label0: Int): (Int, LProgram) = {
      /*
       * L0: Split L1, L3
       * L1: Rを受理する命令列
       * L2: Jump L0
       * L3
       */
      val label1 = label0 + 1 // Split(L1, L3)
      val (label2, program) = r._compile(label1)
      val label3 = label2 + 1 // Jump(L0)

      val split = List(Split(label1, label3))
      val jump = List(Jump(label0))

      (label3, split ++ program ++ jump)
    }
  }

  trait VM {
    type ProgramCounter = Int
    type StringIndex = Int

    def execute(program: Program, s: String): Boolean
  }

  object RecursiveBacktrackingVM extends VM {
    def execute(program: Program, s: String): Boolean = {
      def _execute(pc: ProgramCounter, i: StringIndex): Boolean = {
        program(pc) match {
          case Character(c)          => i < s.size && s(i) == c && _execute(pc+1, i+1)
          case Jump(label)           => _execute(label, i)
          case Split(label1, label2) => _execute(label1, i) || _execute(label2, i)
          case Match                 => i == s.size
        }
      }
      _execute(0, 0)    // (ProgramCounter = 0: 命令列の最初の命令、i = 0: 文字列の最初の文字)
    }
  }

  object IterativeBacktrackingVM extends VM {
    import scala.collection.mutable.Queue

    def execute(program: Program, s: String): Boolean = {
      // threads は (PC, i) のキューで、今後実行すべき実行を蓄えている。
      // IBVM では threads から、スレッドを取り出して実行することを繰り返す。
      // Split するとキューにはふたつのスレッドが追加される。
      // 命令の実行に失敗して死んだスレッドはキューから取り除かれる。
      var threads = Queue[(ProgramCounter, StringIndex)]((0, 0))

      object MatchFailure extends Exception
      while (!threads.isEmpty) {
        val (pc, i) = threads.dequeue()

        try {
          program(pc) match {
            case Character(c) => {
              if (i < s.size && s(i) == c) threads.enqueue((pc + 1, i + 1))
              else throw MatchFailure
            }
            case Jump(label) => threads.enqueue((label, i))
            case Split(label1, label2) => {
              threads.enqueue((label1, i))
              threads.enqueue((label2, i))
            }
            case Match => {
              if (i == s.size) return true
              else throw MatchFailure
            }
          }
        } catch { case MatchFailure => () }
      }
      false
    }
  }

  // 上述のコードでスレッド生成を抑制するための最適化を施したもの。
  object IterativeBacktrackingVM2 extends VM {
    import scala.collection.mutable.Queue

    def execute(program: Program, s: String): Boolean = {
      var threads = Queue[(ProgramCounter, StringIndex)]((0, 0))

      object MatchFailure extends Exception
      while (!threads.isEmpty) {
        val (_pc, _i) = threads.dequeue()
        var pc  = _pc
        var i   = _i

        try {
          while (true) {
            program(pc) match {
              case Character(c) => {
                if (i < s.size && s(i) == c) {
                  pc += 1
                  i += 1
                } else throw MatchFailure
              }
              case Jump(label) => pc = label    // スレッドを生成するかわりに現在のスレッドが実行を継続する
              case Split(label1, label2) => {
                threads.enqueue((label2, i))
                pc = label1                     // label1 に該当するスレッドを生成するかわりに現在のスレッドが実行を継続する
              }
              case Match => {
                if (i == s.size) return true
                else throw MatchFailure
              }
            }
          }
        } catch { case MatchFailure => () }
      }
      false
    }
  }

  /* UNIX の設計者のひとり Ken Thompson による実装です。計算量は入力列について線形。
   * Ken Thompson, "Regular expression search algorithm," Communications of the ACM 11(6),
   * (Jun 1968), pp. 419-422.
   * http://dx.doi.org/10.1145/363347.363387 (学内ネットワークから論文を参照できます)
   */
  object KenThompsonVM extends VM {

    def execute(program: Program, s: String): Boolean = {
      //printProgram(program)
      def endOfLine(i: Int): Boolean = i == s.length

      // threads は文字列の i 文字目を処理したがっているスレッドたちが実行しようとしている命令の PC の集合
      // 最初は命令列の最初の命令に相当する 0 に初期化
      var threads = Set[ProgramCounter](0)
      var done = false

      /*
       * 注意：以下のループは [0, s.length-1] でなく [0, s.length] の範囲を回る。
       * i = s.length で表される行末で Match 命令を実行したときに受理する。
       */
      for (i <- 0 to s.length) {
        var nextThreads = Set[ProgramCounter]()  // i+1文字目を処理したがっているスレッドたち

        while (!done && !threads.isEmpty) {
          val pc = threads.head
          threads = threads - pc
          //println((pc, program(pc)))
          program(pc) match {
            case Character(_c) => if (i < s.length && s(i) == _c) nextThreads = nextThreads + (pc + 1)
            case Jump(label) => threads = threads + label
            case Split(label1, label2) => threads = threads + label1 + label2
            case Match => done = endOfLine(i)
          }
        }
        threads = nextThreads
      }

      done
    }
  }
}
