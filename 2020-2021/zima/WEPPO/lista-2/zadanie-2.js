/*
 * Użycie operatorów . oraz [] do odwoływania się do składowych obiektu.
 */

let a = { }

a.x = 10
console.log(a['x']) // 10

a['y'] = 'y'
console.log(a.y)

// Jakie są różnice między tymi dwoma sposobami?
// Operator [] pozwala na użycie dowolnych napisów jako kluczy.

a['a 1 *'] = []
console.log(a['a 1 *'])
// console.log(a.a 1 *) // nie da się


/*
 * Użycie argumentów innego typu niż string dla operatora [] dostępu do składowej obiektu.
 */

// Co się dzieje jeśli argumentem operatora jest liczba?
// Liczba zostaje zamieniona na napis.
let b = { }
b[1] = 1
b["1"] = 2
console.log(b)

// Co się dzieje jeśli argumentem operatora jest inny obiekt?
// Obiekt zostaje zamieniony na napis.
b[a] = 2
console.log(b)

b[{}] = 3
console.log(b)

b["[object Object]"] = {}
console.log(b)

b[new Number(10)] = new Number(10)
console.log(b)

let obj = {}
obj.toString = () => { "0" }
b[0] = 100
b[obj] = 101
console.log(b)

// Jaki wpływ na klucz pod jakim zapisana zostanie wartość ma programista w obu przypadkach?
// W przypadku obiektów klucz zależy od metody toString, którą programista może zdefiniować.
// W przypadku liczb programista jest zdany na wbudowaną konwersję wartości numerycznych na napisy.

/*
 * Użycie argumentów innego typu niż number dla operatora [] dostępu do tablicy
 */

// Co się dzieje jeśli argumentem operatora jest napis?

let array = [2, 1, 3, 7]

console.log(array["2"]) // Napis rzutowany jest na liczbę jeżeli jest to możliwe.

array["0"] = "0"
console.log(array)

array["a"] = "b" // Klucz jest dodawany jak do zwykłego obiektu.
console.log(array)
console.log(array.length) // Długość tablicy pozostaje bez zmian.

// Co się dzieje jeśli argumentem operatora jest inny obiekt?

array[{}] = {}
console.log(array) // zachowanie się obiektów jako kluczy jest identyczne jak w przypadku obiektów

array[new Number(0)] = 1000000 // Number traktowany jest jako wartość numeryczna
console.log(array)

// Czy i jak zmienia się zawartość tablicy jeśli zostanie do niej dopisana właściwość
// pod kluczem, który nie jest liczbą?
// Tak jak wyżej: do tablicy dodawane są nowe atrybuty.

// Czy można ustawiać wartość atrybutu length tablicy na inną wartość niż liczba
// elementów w tej tablicy? Co się dzieje jeśli ustawia się wartość mniejszą niż liczba
// elementów, a co jeśli ustawia się wartość większą niż liczba elementów?

array.length = 1
console.log(array) // Dalsze elementy zostały ucięte

array.length = 1000
console.log(array) // Tablica zostaje rozszerzona o "puste" elementy <999 empty items>
console.log(array[2]) // undefined
