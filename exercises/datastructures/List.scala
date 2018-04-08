sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, list: List[A]) extends List[A]

object List { // `List` companion object. 
    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x,xs) => x + sum(xs)
    }
    def product(ints: List[Double]) : Double = ints match {
        case Nil => 1
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)
    }
    def apply[A](as: A*): List[A] = // Variadic function syntax
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    // Exercise 3.2
    // implement tail at constant time
    def tail[A](l: List[A]): List[A] = 
        l match {
            case Nil => sys.error("tail of empty list")
            case Cons(x,xs) => xs
        }

    // Exercise 3.3
    // implement setHead for replacing the first element
    def setHead[A](l: List[A], h: A): List[A] = 
        l match {
            case Nil => sys.error("setting head of empty list")
            case Cons(x, xs) => Cons(h, xs)
        }

    // Exercise 3.4
    // generalize tail to the function `drop` which removes the first n
    // elements from a list with time proportional to number of elements 
    // dropped
    def drop[A](l: List[A], n: Int): List[A] = 
        if (n <= 0) l
        else l match {
            case Nil => Nil
            case Cons(_, xs) => drop(xs, n - 1)
        }

    // Exercise 3.5
    // implement dropWhile which removes elements from the List prefix as long
    // as they match a predicate
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
        l match {
            case Cons(x, xs) if f(x) => dropWhile(xs, f)
            case _ => l
        }
}
