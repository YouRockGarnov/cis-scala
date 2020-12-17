package homeworks.homework1

object  task1 extends App {

  import scala.annotation.tailrec
  @tailrec
  def tailNarayanaCows(n: Int, counter: Int, a_0: Int, a_1: Int, a_2: Int): Int =
    counter match {
      case c if c < n => tailNarayanaCows(n, counter + 1, a_1, a_2, a_0 + a_2)
      case c if c >= n => a_0 + a_2
    }

  /**
   * Напишите хвосторекурсивную функцию, возвращающую n-e число из последовательности коров Нараяны,
   * задаваемой отношением
   *
   * a_0 = a_1 = a_2 = 1
   * a_n = a_{n-1} + a_{n-3}
   *
   * https://oeis.org/A000930
   *
   * @param n номер числа последовательности
   * @return n-ое число последовательности коров Нараяны (согласно формуле выше)
   */

  def narayanaCows(n: Int): Int = {
    n match {
      case m if m < 3 => 1
      case m => tailNarayanaCows(m, 3, 1, 1, 1)
    }
  }

  (for (i <- 0 until 10) yield s"$i) ${narayanaCows(i)}").foreach(println)
  //  0) 1
  //  1) 1
  //  2) 1
  //  3) 2
  //  4) 3
  //  5) 4
  //  6) 6
  //  7) 9
  //  8) 13
  //  9) 19
}
