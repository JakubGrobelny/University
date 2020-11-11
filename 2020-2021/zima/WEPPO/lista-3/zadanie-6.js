function createGenerator(n) {
    return function () {
        var _state = 0;
        return {
            next: function () {
                return {
                    value: _state,
                    done:  _state++ >= n
                }
            }
        }
    }
}

var foo10 = {
    [Symbol.iterator]: createGenerator(10)
};

var foo20 = {
    [Symbol.iterator]: createGenerator(20)
}

var foo30 = {
    [Symbol.iterator]: createGenerator(30)
}

console.log("============================")
for (var f of foo10) {
    console.log(f);
}

console.log("============================")
for (var f of foo20) {
    console.log(f);
}

console.log("============================")
for (var f of foo30) {
    console.log(f);
}
