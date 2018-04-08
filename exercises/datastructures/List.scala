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

    // this is how we would call dropWhile
    // $ val xs: List[Int] = List(1,2,3,4,5)
    // $ val ex1 = dropWhile(xs, (x: Int) => x < 4))

    // the problem is that the anon function above needs to specify type
    // in `dropWhile` we already know that is passed a List[Int], the anon
    // function will therefore also take an Int

    // this is solved by currying
    def dropWhileCurried[A](as: List[A])(f: A => Boolean): List[A] =
        as match {
            case Cons(x, xs) if f(x) => dropWhile(xs, f)
            case _ => as
        }
    
    // this is how we would call dropWhileCurried
    // $ dropWhileCurried(xs)(f)

    // Exercise 3.6
    // implement init that returns all but the LAST element in a list
    // why cant this function be implemented in constant time like tail?
    def init[A](l: List[A]): List[A] = 
        l match {
            case Nil => sys.error("init of empty list")
            case Cons(_, Nil) => Nil
            case Cons(h,t) => Cons(h,init(t))
        }

    // foldRight and simple uses example
    def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
        l match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }

    def sumUsingFoldRight(xs: List[Int]) =
        foldRight(xs, 0)((x, y) => x + y)

    def productUsingFoldRight(xs: List[Double]) =
        foldRight(xs, 1.0)((x, y) => x * y)

    def productUsingFoldRightAndUnderscoreNotation(xs: List[Double]) =
        foldRight(xs, 1.0)(_ * _)

    // Exercise 3.7
    // can product using fold right short circuit if encountering 0.0?
    // nah...

    // Exercise 3.8
    /*
      what happens what you pass Nil and Cons to foldRight?
      $ foldRight(List(1,2,3), Nil)(Cons(_,_))
      steps where xs' is the remaining part of the list:
      $ foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil)(Cons(_,_))
      $ Cons(1, foldRight(xs'), Nil)(Cons(_,_))
      $ Cons(1, Cons(2, foldRight(xs', Nil)(Cons(_,_))
      $ Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil)(Cons(_,_))))
      $ Cons(1, Cons(2, Cons(3, Nil)))
    */

    // Exercise 3.9
    // compute length using foldRight
    def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

    // Exercise 3.10
    /* 
        create foldLeft that is tail recursive
        remember: tail call elimination occurs when theres no additional work
        left to do after the recursive call returns
        for example:
        go(n-1,n*acc) is in tail position
        1 + go(n-1, n*acc) is NOT in tail position because of the (1 + go...)
    */
     @annotation.tailrec
     def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = 
         as match {
             case Nil => z
             case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
         }

    // Exercise 3.11
    // write sum, product and length using foldLeft
    def sumUsingFL[A](l: List[Int]) = foldLeft(l, 0)(_ + _)
    def productUsingFL[A](l: List[Double]) = foldLeft(l, 1.0)(_ * _)
    def lengthUsingFL[A](l: List[A]): Int = foldLeft(l, 0)((acc, h) => acc + 1)

    // Exercise 3.12
    // implement reverse using fold
    // def reverseUsingFR[A](l: List[A]): List[A] = 
    //     foldRight(l, Nil)((x,y) =>Cons(y,x))

    def reverse[A](l: List[A]): List[A] = 
        foldLeft(l, List[A]())((x, y) => Cons(y, x))

    // Exercise 3.13
    // write foldRight in terms of foldLeft
    def foldRightUsingFL[A, B](l: List[A], z: B)(f: (A, B) => B): B = 
        foldLeft(reverse(l), z)((x, y) => f(y, x)

    // write foldLeft in terms of foldRight HARD!
    // unsolved
    // def foldLeftUsingFR[A, B](l: List[A], z: B)(f: (A, B) => B): B
    //     = foldRight(reverse(l), z)((x, y) => f(y, x))

    // Exercise 3.14
    // implement append with a fold
    def appendWithFold[A](l: List[A], ll: List[A]): List[A] =
       foldRight(l, ll)(Cons(_,_)) 

    // Exercise 3.15
    // implement concat for concatenating a list of lists into a single list
    // given:
    //  val x = List(List(1,2,3), List(4,5), List(6))
    // return:
    //  List(1,2,3,4,5,6) 

    // TODO

}

// resources:
// https://github.com/theptrk/fpis/blob/master/exercises/ch03/List.scala
