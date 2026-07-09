package it.unibo.pps.tasks.adts

import it.unibo.pps.tasks.adts.SchoolModel.BasicSchoolModule.{SchoolImpl, emptySchool, teacher}
import it.unibo.pps.u03.extensionmethods.Sequences.Sequence
import org.junit.Assert.assertEquals
import org.junit.Test

class Ex2SchoolModelTest:

  val school: SchoolImpl = emptySchool

  @Test def testInitialEmptySchool(): Unit = {
    assertEquals(Sequence.Nil(), school.courses)
    assertEquals(Sequence.Nil(), school.teachers)
  }
