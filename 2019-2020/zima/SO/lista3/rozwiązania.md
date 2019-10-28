# Spis treści

- [Zadanie 1](#zadanie-1)
- [Zadanie 2](#zadanie-2)
- [Zadanie 3](#zadanie-3)
- [Zadanie 4](#zadanie-4)
- [Zadanie 5](#zadanie-5)
- [Zadanie 6](#zadanie-6)
- [Zadanie 7](#zadanie-7)

# Zadanie 1

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

# Zadanie 2

### Wyjaśnij różnice w tworzeniu procesów w systemie Linux(§10.3.3) i WinNT(§11.4.3).

- Linux:
    1. Zostaje utworzony nowy *process descriptor* (`task_struct`) dla
    procesu-dziecka. Większość danych przepisywana jest od rodzica.
    2. Dziecko otrzymuje PID
    3. Tworzona zostaje mapa pamięci i nadawany dzielony
    dostęp do plików rodzica
    4. Rejestry zostają wypełnione odpowiednimi informacjami.

- WinNT – W WinNT procesy są tworzone w inny sposób niż w na Linuxie.
    Zamiast wywołania `fork()` używane jest wywołanie `CreateProcess`,
    które tworzy całkiem nowy proces, który wykonuje program podany
    jako argument. Proces-dziecko nie otrzymuje kopii pamięci
    swojego rodzica (`CreateProcess` działa trochę jak połączenie
    `fork` z `execve`).

### Naszkicuj przebieg akcji podejmowanych przez jądro w trakcie obsługi funkcji `fork(2)` i `execve(2)`.

Kiedy `fork` zostaje wywołany, kernel tworzy `task_struct` i struktury
danych takie jak *kernel-mode stack* czy `thread_info`. Struktury
te są alokowane w ustalonej odległości od końca stosu procesu i
zawierają różne parametry procesu razem z adresem deskryptora procesu.
Następnie szuka się dostępnego PID i uaktualnia tablicę haszującą 
tak, żeby PID wskazywało na `task_struct`a. Na koniec kopiuje¹ się
pamięć rodzica.

Wywołanie `exec` sprawia, że jądro szuka danego polecenia, kopiuje
argumenty i zmienne środowiskowe i zwalnia starą przestrzeń adresową
i tablicę stron. Tworzona zostaje nowa przestrzeń adresowa ale (w
większości systemów) nie zostaje ona uzupełniona (być może z wyjątkiem
strony dla stosu). Rozpoczęcie wykonywania procesu skutkuje w
*page fault*, który powoduje, że pierwsza strona kodu zostaje
wtoczona z pliku wykonywalnego. W ten sposób nic nie musi zostać
załadowane z wyprzedzeniem przez co programy mogą uruchamiać się
szybko. Na koniec argumenty i zmienne środowiskowe zostają skopiowane
na stos, sygnały zostają zresetowane a rejestry wypełnione zerami.

![zad2](zad2.png)

### Wyjaśnij jak system uniksowy optymalizuje klonowanie procesów z użyciem *kopiowania przy zapisie*¹.

System uniksowy optymalizuje klonowanie procesów poprzez odraczanie
kopiowania pamięci rodzica do momentu, kiedy dziecko próbuje do niej
pisać. Wykorzystuje się **kopiowanie przy zapisie** – nowo utworzony
proces otrzymuje swoją tablicę stron, ale wszystkie oznaczone są
jako *read only*. Kiedy dziecko próbuje pisać do pamięci, następuje
*protection fault*. Jądro w takiej sytuacji alokuje nową kopię
strony dla procesu (*read/write*) i dopiero wtedy kopiuje do niej pamięć.

# Zadanie 3

### Na podstawie sekcji 4 publikacji przedstaw pobieżnie argumentację autorów przeciwko `fork`.

- `fork` nie jest już prosty – specyfikacja POSIX wymienia 25 specjalnych
    przypadków kopiowania stanu rodzica do dziecka (*file locks*, timery,
    asynchroniczne IO, *tracing* itp.). W dodatku wiele flag wpływa na
    zachowanie `fork` w kwestii:
        - mapowania pamięci (flagi `madvise` linuksa `MADV_DONTFORK`/
        `DOFORK`/`WIPEONFORK` (`madvise` – wywołanie systemowe 
        informujące system pamięci wirtualnej systemu operacyjnego o 
        planowanym sposobie użycia danego obszaru pamięci) 
        - deskryptorów plików (`O_CLOEXEC` – dzieci nie dziedziczą
        deskryptorów plików, `FD_CLOEXEC` – zamknij plik po wywołaniu
        funkcji z rodziny `exec`)
        - wątków (`pthread_atfork` - rejestruje *fork handlery*, które
        mają zostać wykonane podczas wywołania `fork` w kontekście
        wątku, który wywołał `fork`).
- `fork` nie jest „składalny” – na przykład przez duplikowanie całej
    przestrzeni adresowej przy buferowanym IO użytkownik musi
    eksplicite s*flushować* IO przed `fork`iem aby uniknąć duplikacji
    zawartości bufora. Powoduje to, że `fork` jest kiepską abstrakcją.
- `fork` nie jest *thread-safe* - dziecko utworzone przez `fork` ma
    jedynie jeden wątek. Możliwe jest, że przestrzeń adresowa dziecka 
    nie będzie spójną kopią przestrzeni rodzica. Na przykład gdy
    jeden wątek dokonuje alokacji pamięci i posiada blokadę sterty a 
    inny wątek wywoła `fork`, alokacja pamięci w nowym dziecku
    będzie niemożliwa gdyż będzie ono czekało na odblokowanie, które
    nigdy nie nastąpi. `malloc` nie jest bezpieczny w połączeniu
    z `fork`.
- `fork` jest niebezpieczny – dziecko dziedziczy wszystkie
    uprawnienia po rodzicu a programista jest odpowiedzialny za
    usunięcie wszystkich rzeczy (np. deskryptorów plików), których
    dziecko nie potrzebuje. Zachowanie `fork`a łamię *principle of
    least privilege*.
- `fork` jest powolny – np. przeglądarka Chrome doświadcza
    czasów `fork`owania rzędu 100ms. Słaba wydajność powoduje,
    że np. `posix_spawn` nie wykorzystuje zazwyczaj `fork` a
    Solaris implementuje `spawn` jako natywny syscall.
- `fork` nie jest skalowalny – sposobem do stworzenia skalowalnego
    systemu jest unikanie niepotrzebnego dzielenia. S`fork`owany
    proces zdieli wszystko ze swoim rodzicem.
- `fork` prowadzi do przesadnej alokacji pamięci (*memory overcommit*) –
    gdy duży proces wywołuje `fork` należy stworzyć wiele mapowań
    stron *copy-on-write*, które prawdopodobnie nigdy nie zostaną
    zmodyfikowane. Na Linuxie nie jest to problemem, bo
    domyślną strategią jest *overcommit virtual memory* (??!).

Podsumowanie: `fork` jest wygodnym API dla jednowątkowych procesów 
używającym małej ilości pamięci, które nadaje się do implementacji
powłók, ale większość programów powłokami nie jest.

### Opowiedz jak `vfork(2)` i `posix_spawn(3)` pomagają zniwelować niektóre z wymienionych wad.

- `vfork(2)` – tworzy nowy proces, który dzieli przestrzeń adresową
    rodzica dopóki dziecko wezwie `exec`. Pozwala to na podobny styl
    użycia jak `fork` gdzie nowy proces modyfikuje stan kernela
    przed wywołaniem `exec`a. Przez dzielenie pamięci jest trudno
    używać go bezpiecznie ale unika się kopiowania przestrzeni
    adresowej dzięki czemu zyskuje się na wydajności.
 - `posix_spawn(3)` – tworzy nowy proces-dziecko, który wykonuje
    wskazany plik (połączenie `fork` i `exec`). API `posix_spawn`
    ułatwia refaktorowanie kodu, który zawierał `fork` i `exec`,
    które być może znajdowały się w odległych miejscach w kodzie.
    Wywołanie `close()` po wywołaniu `fork` może zostać zastępione
    przez *pre-spawn call*, które zapisuje, że w dziecku ma odbyć
    się zamknięcie danego pliku.
    
# Zadanie 4

### Który sygnał jest wysyłany domyślnie? 

- kill: `Program received signal SIGTERM, Terminated. 0x00007ffff7b819b7 
    in poll () from /usr/lib/libc.so.6`
- pkill: Z manuala: „pkill will send the specified signal (by default   
    SIGTERM).” `xeyes` najprawdopodobniej ignoruje SIGTERM więc się
    nie zamyka.
- xkill: `X connection to :1 broken (explicit kill or server shutdown).`
    Z manuala:
    „This  command  does  not provide any warranty that the application 
    whose connection to the X server is closed will abort nicely, or even abort at all. All this command does is to
    close the connection to the X server. Many existing applications 
    do indeed abort when their connection to the X server is closed, 
    but some can choose to continue.”
    
### Przy pomocy kombinacji klawiszy `CTRL+Z` wyślij `xeyes` sygnał `SIGSTOP`, a następnie wznów jego wykonanie. Przeprowadź inspekcję pliku `/proc/pid/status` i wyświetl maskę sygnałów oczekujących na dostarczenie. 

**sygnały oczekujące** - sygnały, których dostarczenie jest wstrzymane
dopóki proces nie wyjdzie z nieprzerywalnego snu.

```
    ps -o pid,cmd,stat
    cat /proc/[pid]/status
    kill -l
    kill -SIGUSR1 [pid]
    cat /proc/[pid]/status
    kill -SIGUSR2 [pid]
    cat /proc/[pid]/status
    kill -SIGHUP  [pid]
    cat /proc/[pid]/status
    kill -SIGINT  [pid]
    cat /proc/[pid]/status
```

`SIGUSR1/SIGUSR2` – sygnały zdefiniowane przez użytkownika
`SIGHUP` („signal hang up”) – sygnał wysyłany do procesu kiedy
    jego terminal zostanie zamknięty.
`SIGINT` – przerwanie z klawiatury

### Co opisują pozostałe polapliku «status» dotyczące sygnałów?

- SigQ – liczba zakolejkowanych sygnałów *real user ID* danego procesu
    oraz limit zakolejkowanych sygnałów dla tego procesu.
- SigPnd - liczba sygnałów oczekujących na wątek
- ShdPnd - liczba sygnałów oczekujących na cały proces
- SigBlk, SigIgn, SigCgt – maski sygnałów blokowanych,
ignorowanych, złapanych.

### Który sygnał zostanie dostarczony jako pierwszy po wybudzeniu procesu?

Jako pierwszy dostarczony zostanie `SIGHUP` (znajdował się na prawym
końcu w masce sygnałów oczekujących).

# Zadanie 5

- sierota – proces, który starcił rodzica. Zostaje zaadotpowany
przez proces wyżej w hierarchii.
- żniwiaż – proces, który wyszukuje zombie i odczytyuje ich kod wyjścia.
- kod wyjścia – wynik zakończenia programu zwracany do środowiska i
zapisywany w zmiennej `$?` (w bashu). 0 – sukces, inna wartość – błąd.