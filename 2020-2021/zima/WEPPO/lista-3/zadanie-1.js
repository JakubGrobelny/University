let obj = {
    n : 0,

    get foo() {
        return obj.n;
    },

    set foo(n) {
        obj.n = n
    },

    someMethod : function() {
        console.log(obj.n)
    }
}

obj.foo = 100

// Nowe pola/metody można dodawać używając [], operatora . lub Object.defineProperty

obj['m'] = 1

obj.anotherMethod = function() {
    return obj.n + obj.m
}


console.log(obj.anotherMethod())

// Nowe właściwości get/set można dodać jedynie używając defineProperty

Object.defineProperty(obj, 'bar', {
    get : function() {
        return obj.n
    },

    set : function(i) {
        obj.n = i * 100
    }
})

obj.bar = 27
console.log(obj.bar)
