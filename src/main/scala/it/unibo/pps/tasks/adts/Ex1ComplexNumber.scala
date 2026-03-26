package it.unibo.pps.tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumber:

  trait ComplexADT:
    type Complex
    def toComplex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    // Change assignment below: should probably define a case class and use it?
    case class ComplexImpl(re: Double, im:Double)
    type Complex = ComplexImpl
    def toComplex(re: Double, im: Double): Complex = ComplexImpl(re, im)
    extension (complex: Complex)
      def re(): Double = complex.re
      def im(): Double = complex.im
      def sum(other: Complex): Complex = toComplex(complex.re() + other.re(), complex.im() + other.im())
      def subtract(other: Complex): Complex = toComplex(complex.re() - other.re(), complex.im() - other.im())
      def asString(): String = (re(), im()) match
        case (re, 0) => s"${complex.re()}"
        case (0, im) => s"${complex.im()}i"
        case (re, im) => if im > 0 then s"${complex.re()} + ${complex.im()}i" else s"${complex.re()} - ${Math.abs(complex.im())}i"
