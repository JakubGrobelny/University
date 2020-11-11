function fibIterator() {
    var [a, b] = [0, 1]
    return {
        next : function() {
            let fib = a;
            [a, b] = [b, a + b];
            return {
                value : fib,
                done  : false
            }
        }
    }
}

var fibIter = {
    [Symbol.iterator]: fibIterator
}

function* fib() {
    var [a, b] = [0, 1]
    while (true) {
        yield a;
        [a, b] = [b, a + b]
    }
}

/*
for (let f of fibIter) {
    console.log(f)
}

for (let f of fib()) {
    console.log(f)
}
 */

// Niemożliwe:
// for (let f of fibIter()) {
//     console.log(f)
// }
