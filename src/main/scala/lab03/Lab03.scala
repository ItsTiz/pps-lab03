package lab03

import u02.Modules.*
import u02.Modules.Person.*

import scala.annotation.tailrec

object Lab03 extends App:

  //Task 1 - svolto da solo

  import u03.Sequences.Sequence.*
  import u03.Sequences.*

  /*
   * Skip the first n elements of the sequence
   * E.g., [10, 20, 30], 2 => [30]
   * E.g., [10, 20, 30], 3 => []
   * E.g., [10, 20, 30], 0 => [10, 20, 30]
   * E.g., [], 2 => []
   */
  @tailrec
  def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
    case Cons(_, t) if n > 0 => skip(t)(n - 1)
    case s => s

  /*
   * Zip two sequences
   * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
   * E.g., [10], [] => []
   * E.g., [], [] => []
   */
  def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
    case (Cons(h, t), Cons(h2, t2)) => Cons((h, h2), zip(t, t2))
    case _ => Nil()

  /*
   * Concatenate two sequences
   * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
   * E.g., [10], [] => [10]
   * E.g., [], [] => []
   */
  def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = s1 match
    case Cons(h, t) => Cons(h, concat(t, s2))
    case Nil() => s2

  /*
   * Reverse the sequence
   * E.g., [10, 20, 30] => [30, 20, 10]
   * E.g., [10] => [10]
   * E.g., [] => []
   */
  def reverse[A](s: Sequence[A]): Sequence[A] =
    @tailrec
    def _reverse(start: Sequence[A], current: Sequence[A]): Sequence[A] = start match
      case Cons(h, t) => _reverse(t, Cons(h, current))
      case Nil() => current

    _reverse(s, Nil())

  /*
   * Map the elements of the sequence to a new sequence and flatten the result
   * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
   * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
   * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
   */
  def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
    case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
    case _ => Nil()

  /*
   * Get the minimum element in the sequence
   * E.g., [30, 20, 10] => 10
   * E.g., [10, 1, 30] => 1
   */

  import u03.Optionals.Optional
  import u03.Optionals.Optional.*

  def min(s: Sequence[Int]): Optional[Int] =
    @tailrec
    def _min(s: Sequence[Int], currentMin: Optional[Int]): Optional[Int] = s match
      case Cons(h, t) if isEmpty(currentMin) || h < orElse(currentMin, h) => _min(t, Optional.Just(h))
      case Cons(_, t) => _min(t, currentMin)
      case Nil() => currentMin

    _min(s, Optional.Empty())

  /*
   * Get the elements at even indices
   * E.g., [10, 20, 30] => [10, 30]
   * E.g., [10, 20, 30, 40] => [10, 30]
   */
  def evenIndices[A](s: Sequence[A]): Sequence[A] = s match
    case Cons(h, t) => Cons(h, evenIndices(skip(t)(1)))
    case Nil() => Nil()

  /*
   * Check if the sequence contains the element
   * E.g., [10, 20, 30] => true if elem is 20
   * E.g., [10, 20, 30] => false if elem is 40
   */
  @tailrec
  def contains[A](s: Sequence[A])(elem: A): Boolean = s match
    case Cons(h, t) if h == elem => true
    case Cons(_, t) => contains(t)(elem)
    case Nil() => false

  /*
   * Remove duplicates from the sequence
   * E.g., [10, 20, 10, 30] => [10, 20, 30]
   * E.g., [10, 20, 30] => [10, 20, 30]
   */
  def distinct[A](s: Sequence[A]): Sequence[A] =
    @tailrec
    def _distinct(s: Sequence[A], distinct: Sequence[A]): Sequence[A] = s match
      case Cons(h, t) if !contains(distinct)(h) => _distinct(t, Cons(h, distinct))
      case Cons(_, t) => _distinct(t, distinct)
      case _ => reverse(distinct)

    _distinct(s, Nil())

  /*
   * Group contiguous elements in the sequence
   * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
   * E.g., [10, 20, 30] => [[10], [20], [30]]
   * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
   */
  def group[A](s: Sequence[A]): Sequence[Sequence[A]] =
    @tailrec
    def _group(s: Sequence[A], groupAcc: Sequence[A], result: Sequence[Sequence[A]]): Sequence[Sequence[A]] = s match
      case Cons(h, t) if contains(groupAcc)(h) || groupAcc == Nil() => _group(t, Cons(h, groupAcc), result)
      case Cons(h, t) => _group(t, Cons(h, Nil()), Cons(groupAcc, result))
      case Nil() if groupAcc != Nil() => reverse(Cons(groupAcc, result))
      case _ => reverse(result)

    _group(s, Nil(), Nil())

  /*
   * Partition the sequence into two sequences based on the predicate
   * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
   * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
   */
  def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) =
    @tailrec
    def _partition(seq: Sequence[A], acc: Sequence[A], acc2: Sequence[A]): (Sequence[A], Sequence[A]) = seq match
      case Cons(h, t) if pred(h) => _partition(t, Cons(h, acc), acc2)
      case Cons(h, t) => _partition(t, acc, Cons(h, acc2))
      case Nil() => (reverse(acc), reverse(acc2))

    _partition(s, Nil(), Nil())

  //Task 2 - svolto da solo
  def courses(s: Sequence[Person]): Sequence[String] =
    Sequence.map(filter(s)(p => !isStudent(p)))(p => orElse(course(p), ""))

  @tailrec
  def foldLeft[A, B](s: Sequence[A])(acc: B)(operator: (B, A) => B): B = s match
    case Cons(h, t) => foldLeft(t)(operator(acc, h))(operator)
    case _ => acc

  def totalCourses(s: Sequence[Person]): Int =
    foldLeft(Sequence.map(courses(s))(e => 1))(0)(_ + _)

  //Task 3 - svolto da solo

  import u03.Streams.Stream
  import u03.Streams.Stream.*

  def fill[A](n: Int)(element: A): Stream[A] = n match
    case n if n > 0 => cons(element, fill(n - 1)(element))
    case _ => empty()

  val fibonacci: Stream[Int] =
    def _fibonacci(a: Int, b: Int): Stream[Int] =
      cons(a, _fibonacci(b, a + b))

    _fibonacci(0, 1)

  def cycle[A](lst: Sequence[A]): Stream[A] =
    def _cycle(current: Sequence[A]): Stream[A] = current match
      case Cons(h, t) => cons(h, _cycle(t))
      case Nil() => _cycle(lst)

    _cycle(lst)

end Lab03