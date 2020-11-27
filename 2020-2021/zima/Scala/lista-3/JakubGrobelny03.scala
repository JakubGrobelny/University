object Utils {
    def isSorted(xs: List[Int], cmp: (Int, Int) => Boolean): Boolean =
        xs match {
            case Nil | List(_) => true
            case (a::b::xs)    => cmp(a,b) && isSorted(b::xs, cmp)
        }

    def isAscSorted(xs: List[Int]) = isSorted(xs, _<_)

    def isDescSorted(xs: List[Int]) = isSorted(xs, _>_)

    def foldLeft[A, B](xs: List[A], acc: B)(f: (B, A) => B): B =
        xs match {
            case Nil   => acc
            case x::xs => foldLeft(xs, f(acc, x))(f)
        }

    def sum(xs: List[Int]) = foldLeft(xs, 0)(_+_)

    def length[A](xs: List[A]) = foldLeft(xs, 0)((len:Int, _) => len + 1)

    def compose[A, B, C](f: B => C, g: A => B) = (x:A) => f(g(x))

    // Note: Provided specification didn't really make sense, because A and B parameters
    //       would have to be Int for repeated(f, 3) = f(f(f(3))) equality to work.
    //       Having B also won't work, since the given function must be able to accept
    //       its own output type as its input, so A = B. Therefore this function
    //       has only one type parameter (A), and composes function f with itself n times.
    def repeated[A](f: A => A, n: Int): A => A = {
        if (n < 0) {
            throw new java.lang.IllegalArgumentException("repeated: n must be positive!")
        }

        var fRepeated = (x:A) => x
        for (i <- 1 to n) {
            fRepeated = f compose fRepeated
        }

        fRepeated
    }

    def curry[A, B, C](f: (A, B) => C) = (a: A) => (b: B) => f(a,b)

    def uncurry[A, B,C](f: A => B => C) = (a: A, b: B) => f(a)(b)

    def unSafe[A](exception: Exception)(expression: =>A): A = {
        try {
            expression
        } catch {
            case e: Exception => println(e); throw exception
        }
    }
}

object App {
    import Utils._

    def main(args: Array[String]): Unit = {
        val ascList  = List(0,1,2,3,4,5)
        val descList = List(5,4,3,2,1,0)

        println(s"isAscSorted($ascList) = ${isAscSorted(ascList)}")
        println(s"isDescSorted($ascList) = ${isDescSorted(ascList)}")
        println(s"isAscSorted($descList) = ${isAscSorted(descList)}")
        println(s"isDescSorted($descList) = ${isDescSorted(descList)}")

        println(s"sum($descList) = ${sum(descList)}")
        println(s"length($ascList) = ${length(ascList)}")

        val add1 = (x: Int) => x + 1
        println(s"(add1 compose add1)(0) = ${(add1 compose add1)(0)}")

        val n = 100
        def repeatedAdd = repeated(add1, n)
        println(s"repeated(add1, $n)(0) = ${repeatedAdd(0)}")
        println(s"repeated(add1, 0)(0) = ${repeated(add1, 0)(0)}")
        println(s"repeated(add1, 1)(0) = ${repeated(add1, 1)(0)}")

        def add2(a: Int, b: Int): Int = a + b
        val curried   = curry(add2)
        val uncurried = uncurry(curried)
        println(s"curry(_+_)(1)(2) = ${curried(1)(2)}")
        println(s"uncurry(curry(_+_))(1,2) = ${uncurried(1,2)}")

        try {
            unSafe(new Exception("Another exception")) { 0/0 }
        } catch {
            case e: Exception => println(s"Exception " + e + " caught!")
        }

        unSafe(new Exception("exception")) {
            println("Very safe")
            println("No exception")
        }
    }
}
