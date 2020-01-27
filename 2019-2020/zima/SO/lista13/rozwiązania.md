# Spis treści

- [Zadanie 1](#zadanie-1)
- Zadanie 2 – brak
- [Zadanie 3 (P)](#zadanie-3)
- [Zadanie 4 (P)](#zadanie-4)
- Zadanie 5 (P) – brak
- [Zadanie 6 (P)](#zadanie-6)

***

# Zadanie 1

### Przeczytaj rozdział 2 artykułu „[*Beautiful concurrency*](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/beautiful.pdf)”. Na podstawie poniższego przykładu wyjaśnij czemu złożenie ze sobą poprawnych współbieżnych podprocedur używających blokad nie musi dać poprawnej procedury (tj. „*locks are not composable”*). 

```java
1   class Account {
2       private int balance;
3       synchronized void withdraw(int n) { balance -= n; }
4       synchronized void deposit(int n)  { balance += n; }
5   }
6   
7   void transfer(Account from, Account to, int amount) {
8       from.withdraw(amount);
9       to.deposit(amount);
10  }
```

Składanie ze sobą współbieżnych podprocedur używających blokad nie musi dać poprawnej procedury jeżeli cała nowa procedura nie ma własnej blokady. W przypadku `transfer` wątek może zostać wywłaszczony pomiędzy wykonaniem dwóch zsynchronizowanych procedur, co skutkować będzie tym, że znajdziemy się w stanie, w którym pieniądze ubyły z konta ale nie zostały jeszcze dodane do żadnego innego (więc inny wątek może zaobserwować niepoprawny stan).
```java
void transfer(Account from, Account to, int amount) {
    from.lock(); to.lock();
    from.withdraw(amount);
    to.deposit(amount);
    from.unlock(); to.unlock();
}
```
Powyższe rozwiązanie nie naprawia problemu, bo umożliwia powstanie deadlocka (np. dwa wątki w tej samej chwili próbują zrobić transfer pomiędzy tymi samymi kontami ale w odwrotnej kolejności).

### Jak poprawić procedurę `transfer`? 

Aby poprawić procedurę `transfer` należy dodać blokadę w taki sposób, który uniemożliwi wystąpienie zakleszczenia. Można uzyskać to poprzez narzucenie konkretnej globalnej kolejności nabywania blokad.
Przykładowe rozwiązanie: najpierw blokujemy wątek 

```java
void transfer(Account from, Account to, int amount) {
    if (from < to) { from.lock(); to.lock();   } 
    else           { to.lock();   form.lock(); }
    
    from.withdraw(amount);
    to.deposit(amount);
    
    from.unlock();
    to.unlock();
}
```

### Czemu według autorów artykułu blokady nie są dobrym narzędziem do strukturyzowania współbieżnych programów?

Blokady nie są dobrym narzędziem do strukturyzowania współbieżnych programów, bo powodują wiele trudności i błędów:
- *nabywanie zbyt małej liczby blokad* – można doprowadzić do sytuacji, gdzie dwa wątki będą mogły uzyskać jednoczesny dostęp do tego samego zasobu
- *nabywanie zbyt dużej liczby blokad* – ogranicza to w najlepszym przypadku współbieżność, a w najgorszym umożliwia powstanie deadlocka.
- *nabywanie złych blokad* – programista musi cały czas myśleć o powiązaniach pomiędzy konkretnymi blokadami a konkretnymi danymi.
- *zła kolejność nabywania blokad* – należy być ostrożnym aby blokady były w dobrej kolejności aby uniknąć deadlocków. Jest to trudne i łatwo popełnić błąd.
- *obsługa błędów* – programista musi zagwarantować, że żaden błąd nie pozostawi systemu w niepoprawnym stanie, oraz że nie spowoduje niezwolnienia jakichś blokad.
- *problem zagubionej pobudki* – łatwo jest zapomnieć o zasygnalizowaniu na zmiennej warunkowej (co spowoduje, że jakiś wątek się nie wybudzi), lub sprawdzić ponownie warunek po przebudzeniu.

Blokady i zmienne warunkowe nie wspierają programowania modularnego bo nie pozwalają na łatwe składanie funkcji. Nie jest możliwe tworzenie zamkniętych abstrakcji, bo często trzeba znać ich dokładne implementacje aby zastosować blokowanie poprawnie.

***

# Zadanie 3

### Program `philosophers` jest błędnym rozwiązaniem problemu „ucztujących filozofów”. Dla przypomnienia: każdy z filozofów przez pewien czas śpi, bierze odpowiednio prawą i lewą pałeczkę, je ryż z miski przez pewien czas i odkłada pałeczki. Twoim zadaniem jest poprawienie procedury `philosopher` tak by rozwiązania było wolne od zakleszczeń i głodzenia.
### UWAGA! W rozwiązaniu studenta wszyscy filozofowie muszą być praworęczni!
### Można wprowadzić dodatkowe semafory, a następnie zainicjować je na początku procedury `main`, oraz dodać linie do procedury `philosophers`. Inne modyfikacje programu są niedopuszczalne.

- [philosophers.c](./programy/philosophers.c)

***

# Zadanie 4 
### Problem obiadujących dzikusów
### Plemię n dzikusów biesiaduje przy wspólnym kociołku, który mieści w sobie m ≤ n porcji gulaszu z niefortunnego misjonarza. Kiedy dowolny dzikus chce zjeść, nabiera sobie porcję z kociołka własną łyżką do swojej miseczki i zaczyna jeść gawędząc ze współplemieńcami. Gdy dzikus nasyci się porcją gulaszu to zasypia. Po przebudzeniu znów głodnieje i wraca do biesiadowania. Może się jednak zdarzyć, że kociołek jest pusty. Jeśli kucharz śpi, to dzikus go budzi i czeka, aż kociołek napełni się strawą z następnego niespełnionego misjonarza. Po ugotowaniu gulaszu kucharz idzie spać.
### W udostępnionym pliku źródłowym `savages.c` należy uzupełnić procedury realizujące programy kucharzai dzikusa. Rozwiązanie nie może dopuszczać zakleszczenia i musi budzić kucharza wyłącznie wtedy, gdy kociołek jest pusty. Do synchronizacji procesów można używać wyłącznie semaforów POSIX.1.

- [savages.c](./programy/savages.c)

***

# Zadanie 6

### Problem palaczy tytoniu 
### Mamy trzy wątki palaczy i jeden wątek agenta. Zrobienie i zapalenie papierosa wymaga posiadania tytoniu, bibułki i zapałek. Każdy palacz posiada nieskończoną ilość wyłącznie jednego zasobu – tj. pierwszy ma tytoń, drugi bibułki, a trzeci zapałki. Agent kładzie na stole dwa wylosowane składniki. Palacz, który ma brakujący składnik podnosi ze stołu resztę, skręca papierosa i go zapala. Agent czeka, aż palacz zacznie palić, po czym powtarza wykładanie składników na stół. Palacz wypala papierosa i znów zaczyna odczuwać nikotynowy głód. 
### Wykorzystując plik `smokers.c` rozwiąż problem palaczy tytoniu. Możesz wprowadzić dodatkowe zmienne globalne (w tym semafory) i nowe wątki, jeśli zajdzie taka potrzeba. Pamiętaj, że palacze mają być wybudzani tylko wtedy, gdy pojawią się dokładnie dwa zasoby, których dany palacz potrzebuje. UWAGA! Modyfikowanie kodu procedury `agent` jest zabronione!

- Rozwiązanie jest [stąd](https://pdfs.semanticscholar.org/ca98/650fba1e06b4b4b119adc554c7d7bdd7f0fa.pdf?fbclid=IwAR1KHDwI68f7g4XTQr1FBRxQw9tKVerfi_rEYtH-YJc0sR-MrABVzosLVJ0)
- [smokers.c](./programy/smokers.c)