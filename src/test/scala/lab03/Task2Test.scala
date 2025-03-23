package lab03;

import org.junit.*
import org.junit.Assert.*
import u03.Sequences.Sequence.*
import u03.Sequences.Sequence
import lab03.Lab03.*
import u02.Modules.Person
import u02.Modules.Person.*

class Task2Test:

  val pSequence: Sequence[Person] =
    Cons(Student("Mario", 2001),
      Cons(Teacher("Roberto", "OOP"),
        Cons(Teacher("Giuseppe", "PPS"),
          Cons(Student("Roberto", 2000), Nil()))))

  val nSequence: Sequence[Int] = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))

  @Test def testCoursesFromNilSequence(): Unit =
    assertEquals(Nil(), courses(Nil()))

  @Test def testCoursesFromSimpleSequence(): Unit =
    assertEquals(Cons("OOP", Cons("PPS", Nil())), courses(pSequence))

  @Test def testFoldLeftFromSimpleSequences(): Unit =
    assertEquals(16, foldLeft(nSequence)(0)(_ + _))
    assertEquals(105, foldLeft(nSequence)(1)(_ * _))
    assertEquals("3715", foldLeft(nSequence)("")(_ + _))

  @Test def testTotalCourses(): Unit =
    assertEquals(2, totalCourses(pSequence))
    assertEquals(0, totalCourses(Nil()))

end Task2Test