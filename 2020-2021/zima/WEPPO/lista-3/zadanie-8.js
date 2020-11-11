function* take(it, n) {
    for (let i = 0; i < n; i++) {
        let next = it.next();
        if (next.done) {
            return;
        } else {
            yield next.value
        }
    }
}

// Z poprzedniego zadania
function* fib() {
    var [a, b] = [0, 1]
    while (true) {
        yield a;
        [a, b] = [b, a + b]
    }
}

let tenFibs = take(fib(), 10);
for (let f of tenFibs) {
    console.log(f)
}
