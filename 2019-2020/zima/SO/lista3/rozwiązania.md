#Zadanie 1

![rysunek](zad1.png)

### Na podstawie rysunku przedstaw *stany procesu* w systemie Linux.
- stany procesu:
    - stopped – proces został zatrzymany i może
        zostać wznowiony jedynie przez inny proces
        (na przykład proces może zostać zatrzymywany
        i wznawiany przez debuggera)
    - running 
        - ready – proces został załadowany do pamięci i oczekuje
            na wykonywanie przez procesor.
        - executing – instrukcje procesu są aktualnie
            wykonywane przez procesor.
    - zombie – proces, którego wykonywanie zostało zakończone
        ale dalej znajduje się w tablicy procesów a jego
        zamknięcie nie zostało jeszcze obsłużone przez proces rodzica.
    - interruptible – stan, w którym proces jest zablokowany i czeka
        na zdarzenie takie jak koniec operacji I/O, dostępności
        zasobów bądź sygnał od innego procesu.
    - uninterruptible - stan, w którym proces jest zablokowany ale
        nie obsługuje żadnych sygnałów. (na przykład czeka
        na obsłużenie page fault)

### Podaj akcje albo zdarzenia wyzwalające zmianę stanów. Które przejścia mogą być rezultatem działań podejmowanych przez: jądro systemu operacyjnego, kod sterowników, proces użytkownika?
- Ready <--> Executing:
    - zaplanowanie wykonania procesu przez
    schedulera (działanie podejmowane przez jądro).
- Executing -> Zombie:
    - dobrowolne zakończenie się procesu przez `exit()`.
    - otrzymanie `SIGKILL`a lub `SIGTERM`a
    - zamknięcie przez system w przypadku deadlocka
    - błąd I/O (przez sterownik urządzenia)
- Stopped <--> Ready:
    - otrzymanie sygnału `SIGSTOP` / `SIGCONT` (od procesu użytkownika)
- Interruptible/Uninterruptible --> Ready:
    - wydarzenie I/O (wybudzane przez jądro)
    - sygnał (tylko w przypadku interruptible)
- Executing -> Interruptible/Uninterruptible:
    - żadanie wykonania jakiegoś I/O

### Wyjaśnij różnice między *snem przerywalnym* i *nieprzerywalnym*.
- sen przerywalny (interruptible): proces reaguje na sygnały, przez
    które może zostać obudzony.
- sen nieprzerywalny (uninterruptible): proces nie obsługuje sygnałów, bo
    na przykład śpi podczas wykonywania jakiegoś syscalla, którego
    nie można przerwać sygnałem.

### Czy proces może *zablokować* lub *zignorować* sygnał `SIGKILL`?
- zablokowanie sygnału – system operacyjny nie dostarcza sygnału do
    procesu dopóki nie zostanie on przez niego odblokowany. Proces
    blokuje sygnał poprzez modyfikację swojej maski sygnałów za
    pomocą `sigprocmask()`.
- zignorowanie sygnału - sytuacja, w której nie został zdefiniowany
    handler dla konkretnego sygnału. Zignorowanie sygnału powoduje
    wznowienie wykonania w momencie, w którym otrzymano sygnał.

Nie da się **blokować** `SIGKILL`a. Zignorowanie
tego sygnału powoduje zabicie procesu.
