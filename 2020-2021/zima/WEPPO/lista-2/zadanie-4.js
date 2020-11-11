/*
    Operator "instanceof" działa jak predykat, pozwalający sprawdzić, czy
    dana wartość jest instancją pewnego typu (konstruktora). Zwraca wartość
    boolowską.
*/

console.log(new String("WEPPO") instanceof String)
console.log("WEPPO" instanceof String)

console.log(new Number(42) instanceof Number)

/*
    Operator "typeof" pozwala na uzyskanie typu danego wyrażenia. Zwraca napis
    reprezentujący ten typ. Zwracany typ to albo wyrażenie typu prostego, albo
    "object".
*/

console.log(typeof 42) // number
console.log(typeof(new Number(42)))

console.log(typeof "qwerty") // string
console.log(typeof(new String("qwerty"))) // object