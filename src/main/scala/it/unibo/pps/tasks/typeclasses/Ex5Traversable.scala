package it.unibo.pps.tasks.typeclasses

import it.unibo.pps.u03.Sequences.Sequence
import Sequence.*
import it.unibo.pps.u03.Optionals.Optional

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  def log[A](a: A): Unit = println("The next element is: "+a)

  def logAll[A](seq: Sequence[A]): Unit = seq match
    case Cons(h, t) => log(h); logAll(t)
    case _ => ()

  trait Traversable[T[_]]:
    def traverse[A](t: T[A])(f: A => Unit): Unit

  def logAll[T[_], A](t: T[A])(using tr: Traversable[T]): Unit =
    tr.traverse(t)(log)

  given Traversable[Optional] with
    def traverse[A](opt: Optional[A])(f: A => Unit): Unit = opt match
      case Optional.Just(a) => f(a)
      case Optional.Empty() => ()

  given Traversable[Sequence] with
    def traverse[A](seq: Sequence[A])(f: A => Unit): Unit = seq match
      case Cons(h, t) => f(h); traverse(t)(f)
      case _ => ()

@main def tryTraversable =
  import it.unibo.pps.tasks.typeclasses.Ex5Traversable.*

  val opt = Optional.Just(1)
  logAll(opt) // The next element is: 1

  val seq = Cons(10.0, Cons(20.0, Cons(30.0, Nil())))
  logAll(seq) // The next element is: 10.0
              // The next element is: 20.0
              // The next element is: 30.0

  val nn = Optional.Empty()
  logAll(nn) // "nothing"
