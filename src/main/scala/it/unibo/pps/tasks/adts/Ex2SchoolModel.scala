package it.unibo.pps.tasks.adts

import it.unibo.pps.u03.extensionmethods.Sequences.Sequence, Sequence.*

/*  Exercise 2: 
 *  Implement the below trait, and write a meaningful test.
 *  Suggestions:
 *  - reuse Sequences and Optionals as imported above
 *  - For other suggestions look directly to the methods and their description
 */
object SchoolModel:

  trait SchoolModule:
    type School
    type Teacher
    type Course

    /**
     * This a factory method for create a teacher from a name
     * e.g.,
     * teacher("John") // => Teacher("John")
     * Note!! The internal representation of a teacher may vary, decide what is the best for you
     * @param name the name of the teacher
     * @return the teacher created
     */
    def teacher(name: String): Teacher
    /**
     * This a factory method for create a course from a name
     * e.g.,
     * course("Math") // => Course("Math")
     * Note!! The internal representation of a course may vary, decide what is the best for you
     * @param name the name of the course
     * @return the course created
     *  */
    def course(name: String): Course

    /**
     * This method should return an empty school, namely a school without any teacher and course
     * e.g.,
     * emptySchool // => School(courses = Nil(), teachers = Nil(), teacherToCourses = Nil())
     * NOTE!! The above is just an example, the internal representation may vary, decide what is the best for you
     * You can store just the teacherToCourses, or having a case class for the school, or whatever you think is the best
     * @return the empty school
     */
    def emptySchool: School

    extension (school: School)
      /**
       * This method should return the list of courses
       * e.g.,
       * emptySchool.courses // => Nil()
       * emptySchool.setTeacherToCourse(teacher("John"), course("Math")).courses // => Cons("Math", Nil())
       * emptySchool
       *  .setTeacherToCourse(teacher("John"), course("Math"))
       *  .setTeacherToCourse(teacher("John"), course("Italian")).courses // => Cons("Math", Cons("Italian", Nil()))
       * Note!! If there are duplicates, just return them once
       * @return the list of courses
       */
      def courses: Sequence[String]
      /**
       * This method should return the list of teachers
       * e.g.,
       * emptySchool.teachers // => Nil()
       * emptySchool.setTeacherToCourse(teacher("John"), course("Math")).teachers // => Cons("John", Nil())
       * val john = teacher("John")
       * emptySchool
       *  .setTeacherToCourse(john, course("Math"))
       *  .setTeacherToCourse(john, course("Italian")).teachers // => Cons("John", Nil())
       * Note!! If there are duplicates, just return them once
       * @return the list of teachers
       */
      def teachers: Sequence[String]
      /**
       * This method should return a new school with the teacher assigned to the course
       * e.g.,
       * emptySchool
       *   .setTeacherToCourse(teacher("John"), course("Math")) // => School(courses = Cons("Math", Nil()), teachers = Cons("John", Nil()), teacherToCourses = Cons(("John", "Math"), Nil()))
       *  */
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      /**
       * This method should return the list of courses assigned to a teacher
       * e.g.,
       * emptySchool.coursesOfATeacher(teacher("John")) // => Nil()
       * emptySchool
       *   .setTeacherToCourse(teacher("John"), course("Math"))
       *   .coursesOfATeacher(teacher("John")) // => Cons("Math", Nil())
       * emptySchool
       *   .setTeacherToCourse(teacher("John"), course("Math"))
       *   .setTeacherToCourse(teacher("John"), course("Italian"))
       *   .coursesOfATeacher(teacher("John")) // => Cons("Math", Cons("Italian", Nil()))
       * @return the list of courses assigned to a teacher
       */
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]
      /**
       * This method should return true if the teacher is present in the school
       * e.g.,
       * emptySchool.hasTeacher("John") // => false
       * emptySchool
       *  .setTeacherToCourse(teacher("John"), course("Math"))
       *  .hasTeacher("John") // => true
       *
       */
      def hasTeacher(name: String): Boolean
      /**
       * This method should return true if the course is present in the school
       * e.g.,
       * emptySchool.hasCourse("Math") // => false
       * emptySchool
       *  .setTeacherToCourse(teacher("John"), course("Math"))
       *  .hasCourse("Math") // => true
       *
       */
      def hasCourse(name: String): Boolean

  object BasicSchoolModule extends SchoolModule:
    case class SchoolImpl(name: String, teacherToCourses: Sequence[(Teacher, Course)])
    case class TeacherImpl(teacherName: String)
    case class CourseImpl(courseName: String)
    override type School = SchoolImpl
    override type Teacher = TeacherImpl
    override type Course = CourseImpl

    def teacher(name: String): Teacher = TeacherImpl(teacherName = name)
    def course(name: String): Course = CourseImpl(courseName = name)
    def emptySchool: School = SchoolImpl(name = "", teacherToCourses = Sequence.Nil())

    /** Removes duplicate values from a Sequence[String], keeping the first occurrence. */
    private def distinct(seq: Sequence[String]): Sequence[String] = seq match
      case Nil()      => Nil()
      case Cons(h, t) => Cons(h, distinct(t.filter(_ != h)))

    extension (school: School)
      def courses: Sequence[String] =
        distinct(school.teacherToCourses.map((_, c) => c.courseName))

      def teachers: Sequence[String] =
        distinct(school.teacherToCourses.map((t, _) => t.teacherName))

      def setTeacherToCourse(teacher: Teacher, course: Course): School =
        def append(seq: Sequence[(Teacher, Course)], elem: (Teacher, Course)): Sequence[(Teacher, Course)] =
          seq match
            case Nil()      => Cons(elem, Nil())
            case Cons(h, t) => Cons(h, append(t, elem))
        school.copy(teacherToCourses = append(school.teacherToCourses, (teacher, course)))

      def coursesOfATeacher(teacher: Teacher): Sequence[Course] =
        school.teacherToCourses
          .filter((t, _) => t == teacher)
          .map((_, c) => c)

      def hasTeacher(name: String): Boolean =
        school.teacherToCourses
          .filter((t, _) => t.teacherName == name) match
          case Nil() => false
          case _     => true

      def hasCourse(name: String): Boolean =
        school.teacherToCourses
          .filter((_, c) => c.courseName == name) match
          case Nil() => false
          case _     => true

@main def examples(): Unit =
  import SchoolModel.BasicSchoolModule.*
  val school = emptySchool
  println(school.teachers) // Nil()
  println(school.courses) // Nil()
  println(school.hasTeacher("John")) // false
  println(school.hasCourse("Math")) // false
  val john = teacher("John")
  val math = course("Math")
  val italian = course("Italian")
  val school2 = school.setTeacherToCourse(john, math)
  println(school2.teachers) // Cons("John", Nil())
  println(school2.courses) // Cons("Math", Nil())
  println(school2.hasTeacher("John")) // true
  println(school2.hasCourse("Math")) // true
  println(school2.hasCourse("Italian")) // false
  val school3 = school2.setTeacherToCourse(john, italian)
  println(school3.teachers) // Cons("John", Nil())
  println(school3.courses) // Cons("Math", Cons("Italian", Nil()))
  println(school3.hasTeacher("John")) // true
  println(school3.hasCourse("Math")) // true
  println(school3.hasCourse("Italian")) // true
  println(school3.coursesOfATeacher(john)) // Cons("Math", Cons("Italian", Nil()))

/** A meaningful test suite for the SchoolModule, covering the empty state,
 * single assignment, multiple assignments to the same teacher, and dedup behavior.
 */
@main def schoolModelTests(): Unit =
  import SchoolModel.BasicSchoolModule.*

  // Empty school: nothing should be present
  val empty = emptySchool
  assert(empty.teachers == Sequence.Nil())
  assert(empty.courses == Sequence.Nil())
  assert(!empty.hasTeacher("John"))
  assert(!empty.hasCourse("Math"))
  assert(empty.coursesOfATeacher(teacher("John")) == Sequence.Nil())

  // Single assignment
  val john = teacher("John")
  val math = course("Math")
  val italian = course("Italian")
  val mary = teacher("Mary")

  val s1 = empty.setTeacherToCourse(john, math)
  assert(s1.teachers == Sequence.Cons("John", Sequence.Nil()))
  assert(s1.courses == Sequence.Cons("Math", Sequence.Nil()))
  assert(s1.hasTeacher("John"))
  assert(s1.hasCourse("Math"))
  assert(!s1.hasCourse("Italian"))
  assert(s1.coursesOfATeacher(john) == Sequence.Cons(math, Sequence.Nil()))

  // Multiple assignments to the same teacher: insertion order preserved, no dup teacher entry
  val s2 = s1.setTeacherToCourse(john, italian)
  assert(s2.teachers == Sequence.Cons("John", Sequence.Nil())) // John listed once
  assert(s2.courses == Sequence.Cons("Math", Sequence.Cons("Italian", Sequence.Nil())))
  assert(s2.coursesOfATeacher(john) == Sequence.Cons(math, Sequence.Cons(italian, Sequence.Nil())))

  // A different teacher assigned to an existing course: course listed once, teacher list grows
  val s3 = s2.setTeacherToCourse(mary, math)
  assert(s3.teachers == Sequence.Cons("John", Sequence.Cons("Mary", Sequence.Nil())))
  assert(s3.courses == Sequence.Cons("Math", Sequence.Cons("Italian", Sequence.Nil()))) // Math not duplicated
  assert(s3.coursesOfATeacher(mary) == Sequence.Cons(math, Sequence.Nil()))
  assert(s3.coursesOfATeacher(john) == Sequence.Cons(math, Sequence.Cons(italian, Sequence.Nil()))) // unaffected

  // Immutability check: original school states untouched by later operations
  assert(empty.teachers == Sequence.Nil())
  assert(s1.courses == Sequence.Cons("Math", Sequence.Nil()))

  println("All tests passed!")