console.log((![]+[])[+[]]+(![]+[])[+!+[]]+([![]]+[][[]])[+!+[]+[+[]]]+(![]+[])[!+[]+!+[]]);

// Podzielmy wyrażenie na części:
    (![]+[])[+[]] +
// ![] == false       negacja "prawdziwej" wartości
// ![]+[] == 'false'  dodanie [] powoduje, że oba argumenty '+' traktowane są jako napisy
// +[] == 0           unarny plus rzutuje swój argument na typ numeryczny
// Mamy zatem wyrażenie równoważne 'false'[0], czyli 'f'
    (![]+[])[+!+[]] +               // 2
// (![]+[]) == 'false'   przeanalizowane wyżej
// +[] == 0              jak wyżej
// !+[] == true          negacja logiczna wartości numerycznej zero tworzy wartość boolowską 'true'
// +!+[] == 1            unarny plus rzutuje 'true' na liczbę 1
// Mamy zatem wyrażenie równoważne 'false'[1], czyli 'a'
    ([![]]+[][[]])[+!+[]+[+[]]] +   // 3
// ![] == false                         przeanalizowane wyżej
// [![]] == [false]                     oczywiste
// [][[]] == undefined                  tablica [] nie ma elementu [[]]-tego, więc zwracane jest undefined
// [![]] + [][[]] == 'falseundefined'   plus zaaplikowany do tablicy rzutuje oba argumenty na napisy
// +[] == 0                             przeanalizowane wyżej
// [+[]] == [0]                         oczywiste
// +!+[] == 1                           przeanalizowane wyżej
// +!+[]+[+[]] == '10'                  dodawanie liczby 1 i tablicy [0] powoduje rzutowanie obu wartości na napisy '1' i '0'
// Mamy zatem wyrażenie równoważne 'falseundefined'['10'], czyli 'i' ('10' jest rzutowane na wartość numeryczną)
    (![]+[])[!+[]+!+[]]             // 4
// ![]+[] == 'false'    przeanalizowane wyżej
// !+[] == true         przeanalizowany wyżej
// !+[]+!+[] == 2       operator + rzutuje wartości boolowskie (true == 1) na wartości numeryczne
// Mamy zatem wyrażenie równoważne 'false'[2]

// Ostatecznie: 'false'[0] + 'false'[1] + 'falseundefined'['10'] + 'false'[2] == 'f' + 'a' + 'i' + 'l' == 'fail'
