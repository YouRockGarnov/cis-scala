package homeworks.homework2

object task2 extends App {
  // Реализуйте метод traverse, который превращает List[Option[A]] в Option[List[A]] по следующим правилам:
  // * если входной список содержит только Some[A], то нужно вернуть Some[List[A]] со списком такой же длины,
  //   где внутри списка лежат распакованные значения из входного списка в том же порядке;
  // * иначе нужно вернуть None.
  def traverse[A](list: List[Option[A]]): Option[List[A]] =
    list match {
      case Some(head) :: rest =>
        traverse(rest) match {
          case Some(processedRest) =>
            Some(head :: processedRest)
          case other => other
        }

      case Nil => Some(Nil)
      case None :: nothing => None
    }

  println(traverse(Some('a') :: Some('b') :: Some('c') :: Nil))
  // Some(List(a, b, c)

  println(traverse(Some('a') :: None :: Some('c') :: Nil))
  // None

  // Реализуйте метод splitEither, который превращает List[Either[A, B]] в (List[A], List[B]) по следующим правилам:
  // * в первом элементе возвращаемой пары должны находиться все значения типа A,
  //   которые в списке находились внутри Left[A, *], порядок не важен;
  // * во втором элементе возвращаемой пары должны находитсья все значения типа B,
  //   которые в списке находились внутри Right[*, B], порядок не важен.
  def splitEither[A, B](list: List[Either[A, B]]): (List[A], List[B]) =
    list match {
      case head :: rest =>
        val (a_list, b_list) = splitEither(rest)
        head match {
          case Left(a_item) => (a_item :: a_list, b_list)
          case Right(b_item) => (a_list, b_item :: b_list)
        }
      case Nil => (Nil, Nil)
    }

  println(splitEither(Right(1) :: Right(2) :: Right(3) :: Nil))
  // (List(), List(3, 2, 1)

  println(splitEither(Left("Failure 1") :: Right(2) :: Left("Failure 3") :: Nil))
  // (List(Failure 3, Failure 1), List(2))

  // Реализуйте метод validate, который превращает List[Either[A, B] в Either[List[A], List[B]] по следующим правилам:
  // * если входной список содержит только Right[*, B],
  //   то нужно вернуть Right[*, List[B]] со всеми элементами типа B из входного списка, порядок не важен;
  // * если входной список содержит хотя бы один Left[A, *],
  //   то нужно вернуть Left[List[A], *] со всеми элементами типа A из входного списка, порядок не важен.
  def validate[A, B](list: List[Either[A, B]]): Either[List[A], List[B]] = list match {
    case head :: rest => validate(rest) match {
        case Left(a_list) => head match {
            case Left(a_item) =>
              Left(a_item :: a_list)
            case Right(smth) =>
              Left(a_list)
          }

        case Right(b_list) => head match {
            case Left(a_item) =>
              Left(a_item :: Nil)
            case Right(b_item) =>
              Right(b_item :: b_list)
          }
      }

    case Nil => Right(Nil)
  }

  println(validate(Right(1) :: Right(2) :: Right(3) :: Nil))
  // Right(List(3, 2, 1))

  println(validate(Left("Failure 1") :: Right(2) :: Left("Failure 3") :: Nil))
  // Left(List(Failure 3, Failure 1))
}
