function fibonacciIter(n) {
    let [f0, f1] = [0, 1];
    for (i = 0; i < n; i++) {
        [f0, f1] = [f1, f0 + f1];
    }
    return f0;
}

function fibonacciRec(n) {
    if (n <= 1) {
        return n;
    }
    return fibonacciRec(n-1) + fibonacciRec(n-2);
}

function measureTime(i, f, label) {
    console.time(label);
    console.log("fib(" + i + ") = " + f(i));
    console.timeEnd(label);
}

const n = 40;
for (let i = 10; i < n; i++) {
    measureTime(i, fibonacciIter, "fibonacciIter");
    measureTime(i, fibonacciRec, "fibonacciRec");
}

// Różnice w pomiarach pomiędzy Node a Chrome są nieznaczne, podczas gdy
// wersja rekurencyjna w Firefoxie liczy się nawet kilkanaście razy wolniej
// dla dużych n.