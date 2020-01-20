#Spis treści

- [Zadanie 1](#zadanie-1)
- Zadanie 2 – brak
- Zadanie 3 – brak
- Zadanie 4 – brak
- Zadanie 5 – brak

***

#Zadanie 1

### Podaj cztery warunki konieczne do zaistnienia zakleszczenia. 

- *Mutual-Exclusion Condition* – zasoby są przydzielane na wyłączność poszczególnym procesom lub są wolne.
- *Hold-and-wait Condition* – proces przetrzymuje dostęp do co najmniej jednego zasobu i żąda dostępu do kolejnego.
- *No-Preemption Condition* – zasoby mogą być jedynie dobrowolnie zwalniane przez procesy, które mają do nich dostęp i nie można odebrać ich siłą.
- *Circular Wait Condition* – istnieje zacyklony ciąg procesów, taki, że każdy proces przetrzymuje zasób, który jest żądany przez kolejny proces w cyklu.

### Przeczytaj §6.6 oraz §32.3, a następnie wyjaśnij w jaki sposób można <u>*przeciwdziałać zakleszczeniom*</u> (ang. *deadlock prevention*)?

- **zapobieganie zakleszczeniom** – system zarządza zasobami w taki sposób, żeby zakleszczenia nie wystąpiły:
    - eliminacja *Mutual-Exclusion Condition* – unikanie wzajemnego wykluczania i przypisywania zasobów na wyłączność o ile nie jest to absolutnie konieczne. Dostęp do danych, które są tylko do odczytu nadajemy wielu procesom na raz. W przypadku zasobów do pisania, można stosować np. *spooling* – dane wymagające przetworzenia przez urządzenie są umieszczane w buforze na dysku lub w pamięci.
    - eliminacja *Hold-and-wait Condition* – wszystkie procesy żądają wszystkich potrzebnych zasobów przed rozpoczęciem działania lub wszystkie zasoby nabywane są atomowo. Alternatywnie: zwalniamy poprzednie blokady jeżeli nie możemy zdobyć kolejnych.
    - eliminacja *No-Preemption Condition* – wirtualizacja zasobów, która umożłiwa wywłaszczanie procesów w trakcie korzystania z zasobów.
    - eliminacja *Circular Wait Condition* – wprowadzenie zasady, że procesy mają prawo do tylko jednego zasobu jednocześnie. Alternatywnie: numerujemy zasoby. Procesy mogą uzyskiwać dostęp do zasobów o numerach w kolejności rosnącej. Jeżeli chcą inne zasoby, to muszą zwolnić te przetrzymywane.

### Które z proponowanych rozwiązań stosuje się w praktyce i dlaczego? Czemu pozostałe są zbyt kłopotliwe?

- stosowane: 
    - *spooling* (ang.*simultaneous peripheral operations on-line*) w przypadku drukarek
    - *partial lock ordering* (eliminacja *Circular Wait Condition*) przy mapowaniu pamięci w Linuksie.
- kłopotliwe:
    - eliminacja *hold-and-wait*: konieczność znajomości wszystkich zasobów, które będą potrzebne. Zasoby muszą być nabywane wcześnie, zamiast wtedy, gdy są faktycznie potrzebne. W przypadku, gdy zwalniamy wcześniej zdobyte blokady, możemy doprowadzić do *livelock*a.
    – eliminacja *mutual-exclusion*: w przypadku drukarek możliwe jest, że proces, dla którego rozpoczęto drukowanie nie dostarczył jeszcze wszystkich danych do bufora. Jeżeli oczekuje się na wszystkie dane przed rozpoczęciem drukowania, to możlliwe jest przepełnienie bufora.
    - eliminacja *no-preemption*: nie wszystkie zasoby dają się zwirtualizować.
    


***