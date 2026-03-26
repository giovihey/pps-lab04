package it.unibo.pps.tasks.adts

import org.junit.*
import org.junit.Assert.*
import Ex1ComplexNumber.*

/* Tests should be clear, but note they are expressed independently of the 
   specific implementation  -- UNCOMMENT FOR THE EXERCISE!
*/
class ComplexNumberTests:

  // Choice of implementation to test
  val toComplexADT: ComplexADT = BasicComplexADT
  import toComplexADT.*

  // From now, everything is independent of specific implementation of Complex

  @Test def testReal() =
    assertEquals(10, toComplex(10, 20).re(), 0)

  @Test def testImaginary() =
    assertEquals(20, toComplex(10, 20).im(), 0)

  @Test def testSum() =
    assertEquals(toComplex(11, 22), toComplex(10, 20) sum toComplex(1, 2))

  @Test def testSubtract() =
    assertEquals(toComplex(9, 18), toComplex(10, 20) subtract toComplex(1, 2))

  @Test def testAsString() =
    assertEquals("10.0 + 5.0i", toComplex(10.0, 5.0).asString())

  @Test def optionalTestAdvancedAsString() =
    assertEquals("0.0", toComplex(0, 0).asString())
    assertEquals("10.0", toComplex(10.0, 0).asString())
    assertEquals("10.0 + 5.0i", toComplex(10.0, 5.0).asString())
    assertEquals("10.0 - 5.0i", toComplex(10.0, -5.0).asString())
    assertEquals("5.0i", toComplex(0, 5.0).asString())
    assertEquals("-5.0i", toComplex(0, -5.0).asString())
