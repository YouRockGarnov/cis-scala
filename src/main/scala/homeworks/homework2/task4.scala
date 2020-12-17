package homeworks.homework2

object task4 extends App {

  sealed trait BinaryTree[+A] {
    // Реализуйте методы insert и contains для бинарного дерева со значениями произвольного типа,
    // используя тайпкласс scala.Ordering.
    // Сигнатуры методов insert/contains или классов можно (и нужно) модифицировать
    // для работы с неявными параметрами
    def insert[B >: A](newValue: B): BinaryTree[B] = this match {
      case Leaf => Branch(newValue, Leaf, Leaf)
      case Branch(value, left, right) if ord.compare(newValue, value) <= 0 =>
        Branch(value, left.insert(newValue), right)
      case Branch(value, left, right) if ord.compare(newValue, value) > 0 =>
        Branch(value, left, right.insert(newValue))
    }

    def contains[B >: A](wanted: B): Boolean = this match {
      case Branch(value, left, right) if ord.compare(value, wanted) < 0 => right.contains(wanted)
      case Branch(value, left, right) if ord.compare(value, wanted) == 0 => true
      case Branch(value, left, right) if ord.compare(value, wanted) > 0 => left.contains(wanted)
      case Leaf => false
    }

    def dump(depth: Int = 0): Unit =
      this match {
        case Branch(value, left, right) =>
          right.dump(depth + 1)
          println(" " * depth + value)
          left.dump(depth + 1)
        case _ =>
      }
  }

  final case class Branch[A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

  case object Leaf extends BinaryTree[Nothing]

  Leaf
    .insert("DEF")
    .insert("ABC")
    .insert("IJK")
    .insert("FGH")
    .insert("LMN")
    .dump()

  //   LMN
  //  IJK
  //   FGH
  // DEF
  //  ABC
}
