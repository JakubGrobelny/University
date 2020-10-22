//scalar product of two vectors xs and ys
def scalarUgly(xs: List[Int], ys: List[Int]) = {
    var product = 0;
    var it = xs.lazyZip(ys).iterator;
    while (it.hasNext) {
        val (x, y) = it.next();
        product += x * y;
    };
    product
}

def scalar(xs: List[Int], ys: List[Int]) =
    (for ((x,y) <- xs.lazyZip(ys)) yield x * y).sum

//quicksort algorithm
def sortUgly(xs: List[Int]): List[Int] = {
    def partition(xs: List[Int], pivot: Int): (List[Int], List[Int]) = {
        var lesser: List[Int] = Nil;
        var greaterEq: List[Int] = Nil;
        var xsIt = xs.iterator;
        while (xsIt.hasNext) {
            val head = xsIt.next();
            if (head < pivot) {
                lesser  = head :: lesser;
            } else {
                greaterEq = head :: greaterEq;
            }
        }
        (lesser, greaterEq)
    }

    xs match {
        case Nil => Nil
        case x::xs => {
            var (left, right) = partition(xs, x);
            sortUgly(left) ++ (x :: sortUgly(right))
        }
    }
}

def sort(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case x::xs => {
        val right = sort(for (elem <- xs if elem >= x) yield elem);
        val left  = sort(for (elem <- xs if elem <  x) yield elem);
        left ++ (x :: right)
    }
}

//checks if n is prime
def isPrimeUgly(n: Int): Boolean = {
    if (n <= 1) return false;

    var it = (2 to math.sqrt(n).toInt).iterator;

    while (it.hasNext) {
        if (n % it.next() == 0) return false;
    }

    true
}

def isPrime(n: Int): Boolean = {
    if (n <= 1) return false;

    for (i <- 2 to math.sqrt(n).toInt) {
        if (n % i == 0) return false;
    }

    true
}

//for given positive integer n, find all pairs of integers i and j, where 1 ≤ j < i < n such that i + j is prime
def primePairsUgly(n : Int): List[(Int, Int)] = {
    var i = 2;
    var pairs : List[(Int, Int)] = Nil;
    while (i < n) {
        var j = 1;
        while (j < i) {
            if (isPrime(i + j))
                pairs = (i, j) :: pairs;
            j += 1;
        }
        i += 1;
    }
    pairs
}

def primePairs(n : Int): List[(Int, Int)] =
    (for (i <- 2 until n; j <- 1 until i; if isPrime(i + j)) yield (i, j)).toList

//create a list with all lines from given file
val filesHere = new java.io.File(".").listFiles

def fileLinesUgly(file: java.io.File): List[String] = {
    var scanner = new java.util.Scanner(file);
    var lines: List[String] = Nil;
    while (scanner.hasNextLine) {
        lines = scanner.nextLine :: lines;
    }
    lines.reverse
}

def fileLines(file: java.io.File): List[String] = scala.io.Source.fromFile(file).getLines().toList


//print names of all .scala files which are in filesHere & are non empty
def printNonEmptyUgly(pattern: String): Unit = {
    var filesIt = filesHere.iterator;
    while (filesIt.hasNext) {
        val file = filesIt.next();
        val fileName = file.getName;
        if (fileName.endsWith(pattern) && file.length != 0) {
            println(fileName)
        }
    }
}

def printNonEmpty(pattern: String): Unit = {
    for (file <- filesHere; if file.getName.endsWith(pattern) && file.length != 0) {
        println(file.getName);
    }
}
