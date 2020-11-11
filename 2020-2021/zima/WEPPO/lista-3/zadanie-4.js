// Zmienne tworzone z użyciem słowa kluczowego var wiązane są w obrębie
// funkcji a nie bloku, co powoduje, że w każdej iteracji pętli mamy do czynienia
// z tą samą zmienną 'i', podczas gdy przy użyciu let zmienna ta byłaby świeżą
// zmienną w każdej iteracji pętli. Tworzone w tablicy funkcje posiadają
// referencję na tę samą zmienną 'i', która na koniec pętli ma wartość 10.

function createFs(n) {
    var fs = [];
    for (var i = 0; i < n; i++) {
        fs[i] = function(i) { // rozwiązanie: wiążemy zmienną w ciele nowej funkcji
            return function () {
                return i;
            }
        }(i);
    };
    return fs;
}

var myfs = createFs(10);
console.log(myfs[0]());
console.log(myfs[2]());
console.log(myfs[7]());
