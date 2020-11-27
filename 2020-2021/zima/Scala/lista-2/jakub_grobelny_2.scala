package numbers {

    class Rational(n: Int, d: Int = 1) {
        require(d != 0, "Denominator must be non-zero.")

        val (numerator, denominator) = normalize(n, d)

        private def normalize(n: Int, d: Int): (Int, Int) = {
            def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

            val (normN, normD) = if (d < 0) (-n, -d) else (n, d)

            val divisor = gcd(normN.abs, normD)
            (normN / divisor, normD / divisor)
        }

        def ==(other: Rational): Boolean =
            numerator == other.numerator && denominator == other.denominator

        def !=(other: Rational): Boolean = !(this == other)

        def +(other: Rational): Rational = {
            val newNum = numerator * other.denominator + denominator * other.numerator
            val newDen = denominator * other.denominator
            new Rational(newNum, newDen)
        }

        def -(other: Rational): Rational = {
            val newNum = numerator * other.denominator - denominator * other.numerator
            val newDen = denominator * other.denominator
            new Rational(newNum, newDen)
        }

        def *(other: Rational): Rational = {
            val newNum = numerator * other.numerator
            val newDen = denominator * other.denominator
            new Rational(newNum, newDen)
        }

        def /(other: Rational): Rational = {
            val newNum = numerator * other.denominator
            val newDen = denominator * other.numerator
            new Rational(newNum, newDen)
        }

        override def toString: String = {
            if (numerator == 0) return "0"

            val integralPart     = numerator.abs / denominator;
            val reducedNumerator = numerator.abs % denominator

            val signStr       = if (numerator >= 0)        "" else "-"
            val integralStr   = if (integralPart == 0)     "" else s"$integralPart "
            val fractionalStr = if (reducedNumerator == 0) "" else s"$reducedNumerator/$denominator"

            signStr + (integralStr + fractionalStr).trim
        }

        def toDouble: Double = numerator.toDouble / denominator
    }

    object Rational {
        val zero = new Rational(0)

        val one = new Rational(1)

        def apply(n: Int) = new Rational(n)

        def apply(n: Int, d: Int) = new Rational(n, d)
    }
}


package figures {

    import numbers.Rational

    class Point(val x: Rational, val y: Rational) {
        def ==(other: Point): Boolean = x == other.x && y == other.y

        def !=(other: Point): Boolean = !(this == other)

        def distance(other: Point): Double = {
            val xDiff = x - other.x
            val yDiff = y - other.y
            scala.math.sqrt(((xDiff * xDiff) + (yDiff * yDiff)).toDouble)
        }
    }

    object Point {
        def apply(x: Rational, y: Rational): Point = new Point(x, y)

        def apply(xy: (Int, Int)): Point = new Point(Rational(xy._1), Rational(xy._2))

        def apply(x: Int, y: Int): Point = new Point(Rational(x), Rational(y))

        def apply(x: (Int, Int), y: (Int, Int)): Point =
            new Point(Rational(x._1, x._2), Rational(y._1, y._2))
    }

    trait Figure {
        def area: Double
        val description: String
    }

    class Triangle(val p1: Point, val p2: Point, val p3: Point) extends Figure {
        require(p1 != p2 && p2 != p3 && p1 != p3, "All triangle vertices must be unique.")

        val (size1, size2, size3) = (p1.distance(p2), p2.distance(p3), p3.distance(p1))

        require(
            size1 + size2 > size3 && size1 + size3 > size2 && size2 + size3 > size1,
            "Triangle inequality must be met"
        )

        def area: Double =
            (p1.x * (p2.y - p3.y) + p2.x * (p3.y - p1.y) + p3.x * (p1.y - p2.y)).toDouble / 2

        override val description: String = "Triangle"

        def this(p1: (Int, Int), p2: (Int, Int), p3: (Int, Int)) = {
            this(Point(p1), Point(p2), Point(p3))
        }
    }

    // It is assumed that the points are given in either clockwise or counterclockwise order.
    /*
                                        a -------- b
                                        |          |
                                        d----------c
    */
    class Rectangle(val a: Point, val b: Point, val c: Point, val d: Point) extends Figure {
        require(a != b && b != c && c != d && d != a, "All vertices must be unique.")

        val (abSize, cdSize) = (a.distance(b), c.distance(d))
        val (bcSize, daSize) = (b.distance(c), d.distance(a))

        require(abSize == cdSize && bcSize == daSize, "Opposite sides must be equal.")

        val diagonal1 = a.distance(c)
        val diagonal2 = b.distance(d)

        require(diagonal1 == diagonal2, "Both diagonals must be equal.")

        def area: Double = abSize * bcSize

        override val description: String = "Rectangle"

        def this(a: (Int, Int), b: (Int, Int), c: (Int, Int), d: (Int, Int)) = {
            this(Point(a), Point(b), Point(c), Point(d))
        }

        def this(origin: Point, width: Rational, height: Rational) = {
            this(
                origin,
                Point(origin.x + width, origin.y),
                Point(origin.x + width, origin.y + height),
                Point(origin.x, origin.y + height))
        }

        def this(origin: Point, width: Int, height: Int) = {
            this(origin, Rational(width), Rational(height))
        }
    }

    class Square(a: Point, b: Point, c: Point, d: Point) extends Rectangle(a, b, c, d) {
        require(abSize == bcSize && cdSize == daSize, "All sides of a square must be equal")

        override val description: String = "Square"

        def this(a: (Int, Int), b: (Int, Int), c: (Int, Int), d: (Int, Int)) =
            this(Point(a), Point(b), Point(c), Point(d))

        def this(origin: Point, size: Rational) =
            this(
                origin,
                Point(origin.x + size, origin.y),
                Point(origin.x + size, origin.y + size),
                Point(origin.x, origin.y + size)
            )

        def this(origin: Point, size: Int) = this(origin, Rational(size))
    }

    object Figure {
        def areaSum(figures: List[Figure]): Double = (figures map (_.area)).sum

        def printAll(figures: List[Figure]): Unit = for (figure <- figures) println(figure.description)
    }
}


object Example {
    import numbers._;
    import figures._;

    def testRational(): Unit = {
        def testOp(opStr: String, opFn: (Rational, Rational) => Rational, a: Rational, b: Rational) = {
            println(s"$a $opStr $b = ${opFn(a, b)}")
        }

        def testAdd = (a: Rational, b: Rational) => testOp("+", _+_, a, b)
        def testSub = (a: Rational, b: Rational) => testOp("-", _-_, a, b)
        def testMul = (a: Rational, b: Rational) => testOp("*", _*_, a, b)
        def testDiv = (a: Rational, b: Rational) => testOp("/", _/_, a, b)

        testAdd(Rational.zero, Rational.zero)
        testAdd(Rational.one,  Rational.zero)
        testAdd(Rational.one,  Rational.one)
        testAdd(Rational(1,2), Rational(3,4))
        testAdd(Rational(-1),  Rational.one)

        testSub(Rational(1,2), Rational(3,4))
        testSub(Rational(100), Rational(50,2))
        testSub(Rational(1),   Rational(1, 1000))

        testMul(Rational(3,4), Rational(4,7))
        testMul(Rational(3),   Rational(1,3))

        testDiv(Rational(21,37), Rational(21,37))
        testDiv(Rational(3),     Rational(2))
        testDiv(Rational(7),     Rational(1236, 49))

        try {
            val invalidRational = Rational(1000, 0)
        } catch {
            case e : java.lang.IllegalArgumentException => println("Denominator = 0 rejected")
        }
    }

    def testFigures(): Unit = {
        def printArea(figure: Figure) = {
            println(s"${figure.description} area = ${figure.area}")
        }

        val square    = new Square(Point(0,0), 13)
        val rectangle = new Rectangle(Point(0,0), 7, 4)
        val triangle  = new Triangle((0, 0), (10, 0), (0, 10))

        printArea(square)
        printArea(rectangle)
        printArea(triangle)

        val list = List(square, rectangle, triangle)

        println(s"Sum of areas: ${Figure.areaSum(list)}")
        Figure.printAll(list)

        try {
            val invalidTriangle = new Triangle((0, 0), (1, 1), (2, 2))
        } catch {
            case e : java.lang.IllegalArgumentException => println("Invalid triangle rejected")
        }
    }

    def main(args: Array[String]): Unit = {
        println("== Rational ========================")
        testRational()

        println()

        println("== Figures  ========================")
        testFigures()
    }
}

