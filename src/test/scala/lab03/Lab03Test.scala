package lab03;

import org.junit.*
import org.junit.Assert.*
import lab03.Lab03.*
import u02.Modules.Person
import u02.Modules.Person.*

class Task1Test:

  import u03.Sequences.Sequence.*
  import u03.Sequences.*
  import u03.Optionals.Optional.*
  import u03.Optionals.Optional

  val sequence: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum(): Unit =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(sequence))

  @Test def testMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), Sequence.map(sequence)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), Sequence.map(sequence)(_ + ""))

  @Test def testFilter(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), filter(sequence)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(sequence)(_ != 20))

  @Test def testSkip(): Unit =
    assertEquals(Cons(30, Nil()), skip(sequence)(2))
    assertEquals(Nil(), skip(sequence)(3))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), skip(sequence)(0))
    assertEquals(Nil(), skip(Nil())(2))

  @Test def testZip(): Unit =
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zip(sequence, l2))
    assertEquals(Nil(), zip(sequence, Nil()))
    assertEquals(Nil(), zip(Nil(), l2))
    assertEquals(Nil(), zip(Nil(), Nil()))

  @Test def testConcat(): Unit =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concat(sequence, l2))
    assertEquals(Cons(40, Cons(50, Nil())), concat(Nil(), l2))

  @Test def testReverse(): Unit =
    assertEquals(Cons(30, Cons(20, Cons(10, Nil()))), reverse(sequence))
    assertEquals(Nil(), reverse(Nil()))

  @Test def testFlatMap(): Unit =
    assertEquals(Cons(10, Cons(11, Cons(20, Cons(21, Cons(30, Cons(31, Nil())))))), flatMap(sequence)(v => Cons(v, Cons(v + 1, Nil()))))
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(sequence)(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

  @Test def testMin(): Unit =
    assertEquals(Just(10), min(sequence))
    assertEquals(Just(1), min(Cons(10, Cons(30, Cons(20, Cons(1, Cons(2, Cons(5, Nil()))))))))
    assertEquals(Just(1), min(Cons(1, Nil())))
    assertEquals(Empty(), min(Nil()))

  @Test def testEvenIndices(): Unit =
    assertEquals(Cons(10, Cons(30, Nil())), evenIndices(sequence))
    assertEquals(Cons(10, Cons(30, Nil())), evenIndices(Cons(10, Cons(20, Cons(30, Cons(40, Nil()))))))
    assertEquals(Nil(), evenIndices(Nil()))

  @Test def testContains(): Unit =
    assertEquals(true, contains(sequence)(10))
    assertEquals(false, contains(sequence)(15))
    assertEquals(false, contains(Nil())(10))

  @Test def testDistinct(): Unit =
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), distinct(sequence))
    assertEquals(Cons(10, Cons(20, Nil())), distinct(Cons(10, Cons(20, Cons(10, Nil())))))
    assertEquals(Nil(), distinct(Nil()))

  @Test def testGroup(): Unit =
    val sequence = Cons(10, Cons(10, Cons(20, Cons(30, Cons(20, Nil())))))
    val grouped = Cons(Cons(10, Cons(10, Nil())), Cons(Cons(20, Nil()), Cons(Cons(30, Nil()), Cons(Cons(20, Nil()), Nil()))))
    assertEquals(grouped, group(sequence))
    assertEquals(Nil(), group(Nil()))

  @Test def testPartition(): Unit =
    val sequence = Cons(11, Cons(20, Cons(31, Nil())))
    val (even, odd) = partition(sequence)(x => x % 2 == 0)
    assertEquals(Cons(20, Nil()), even)
    assertEquals(Cons(11, Cons(31, Nil())), odd)

    val emptySequence = Nil()
    val (evenEmpty, oddEmpty) = partition(emptySequence)(x => true)
    assertEquals(Nil(), evenEmpty)
    assertEquals(Nil(), oddEmpty)

end Task1Test

class Task2Test:

  import u03.Sequences.Sequence.*
  import u03.Sequences.*

  val pSequence: Sequence[Person] =
    Cons(Student("Mario", 2001),
      Cons(Teacher("Roberto", "OOP"),
        Cons(Teacher("Giuseppe", "PPS"),
          Cons(Student("Roberto", 2000), Nil()))))

  val pSequence2: Sequence[Person] =
    Cons(Teacher("Aldo", "Machine Larning"),
      Cons(Student("Giovanni", 2000),
        Cons(Teacher("Giacomo", "LCMC"),
          Cons(Student("Luigi", 2000),
            Cons(Teacher("Luigi", "PCD"), Nil())))))

  val nSequence: Sequence[Int] = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))

  @Test def testCoursesFromNilSequence(): Unit =
    assertEquals(Nil(), courses(Nil()))

  @Test def testCoursesFromSimpleSequence(): Unit =
    assertEquals(Cons("OOP", Cons("PPS", Nil())), courses(pSequence))
    assertEquals(Cons("Machine Larning", Cons("LCMC", Cons("PCD", Nil()))), courses(pSequence2))

  @Test def testFoldLeftFromSimpleSequences(): Unit =
    assertEquals(16, foldLeft(nSequence)(0)(_ + _))
    assertEquals(105, foldLeft(nSequence)(1)(_ * _))
    assertEquals("3715", foldLeft(nSequence)("")(_ + _))

  @Test def testTotalCourses(): Unit =
    assertEquals(2, totalCourses(pSequence))
    assertEquals(0, totalCourses(Nil()))

end Task2Test


class Task3Test:

  import u03.Streams.*
  import u03.Sequences.Sequence.*
  import u03.Sequences.*

  @Test def testFill(): Unit =
    val str1 = fill(3)("a")
    val str2 = fill(2)("b")
    assertEquals(Cons("a", Cons("a", Cons("a", Nil()))), Stream.toList(str1))
    assertEquals(Cons("b", Cons("b", Nil())), Stream.toList(str2))

  @Test def testFibonacci(): Unit =
    val str1 = Stream.take(fibonacci)(5)
    val str2 = Stream.take(fibonacci)(7)
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Nil()))))), Stream.toList(str1))
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Nil()))))))), Stream.toList(str2))

  @Test def testCycle(): Unit =
    val str1 = Stream.take(cycle(Cons("a", Cons("b", Cons("c", Nil())))))(5)
    val str2 = Stream.take(cycle(Cons(1, Cons(2, Nil()))))(6)
    assertEquals(Cons("a", Cons("b", Cons("c", Cons("a", Cons("b", Nil()))))), Stream.toList(str1))
    assertEquals(Cons(1, Cons(2, Cons(1, Cons(2, Cons(1, Cons(2, Nil())))))), Stream.toList(str2))

end Task3Test
