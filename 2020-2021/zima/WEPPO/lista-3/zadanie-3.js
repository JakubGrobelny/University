function forEach(xs, f) {
    for (let x of xs) {
        f(x)
    }
}

function map(xs, f) {
    let ys = []
    for (let x of xs) {
        ys.push(f(x))
    }
    return ys
}

function filter(xs, f) {
    let ys = []
    for (let x of xs) {
        if (f(x)) {
            ys.push(x)
        }
    }
    return ys
}


var a = [1, 2, 3, 4];

function square(x) {
    return x * x
}


forEach(a, function (x) { console.log(x); }); // [1,2,3,4]

console.log(map(a, square));        // [2,4,6,8]

console.log(filter(a, _ => _ < 3));     // [1,2]
