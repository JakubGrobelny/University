function sum(...xs) {
    return xs.reduce((acc, x) => acc + x, 0)
}

console.log(sum())

console.log(sum(1, 2, 3))

console.log(sum(1, 2, 3, 4, 5))


// alternatywnie:
function sum0() {
    let sum = 0;
    for (let arg of arguments) {
        sum += arg;
    }
    return sum;
}

console.log(sum0(1,1,1,1))
