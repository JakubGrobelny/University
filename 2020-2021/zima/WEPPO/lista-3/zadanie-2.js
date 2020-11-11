function memoize(f) {
    var cache = {}
    return function(n) {
        if (!cache[n]) {
            cache[n] = f(n);
        }
        return cache[n]
    }
}

var fibonacci = memoize(fibonacci)

// Z wcześniejszej listy zadań
function fibonacciIter(n) {
    let [f0, f1] = [0, 1];
    for (i = 0; i < n; i++) {
        [f0, f1] = [f1, f0 + f1];
    }
    return f0;
}

function fibonacci(n) {
    if (n <= 1) {
        return n;
    }
    return fibonacci(n-1) + fibonacci(n-2);
}

function measureTime(i, f, label) {
    console.time(label);
    console.log("fib(" + i + ") = " + f(i));
    console.timeEnd(label);
}

const n = 100;
measureTime(n, fibonacciIter, "fibonacciIter");
measureTime(n, fibonacci, "fibonacci")

// Czas wykonania wersji z memoizacją jest mniejszy od wersji iteracyjnej
