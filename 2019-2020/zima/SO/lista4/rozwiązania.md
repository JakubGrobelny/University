# Spis treści

- [Zadanie 1](#zadanie-1)
- [Zadanie 2](#zadanie-2)
- [Zadanie 3](#zadanie-3)

***
# Zadanie 1

**obraz procesu** (ang. *process image*) – zawartość pliku wykonywalnego załadowana do pamieci.

### Na podstawie podręcznika [execve(2)](https://www.freebsd.org/cgi/man.cgi?query=execve&sektion=2) wyjaśnij, które zasoby są dziedziczone przez świeżo załadowany program. Co dzieje się z otwartymi plikami i obsługą sygnałów? 

- File descriptors open in the calling process image	remain open in the new process image, except for those for which the close-on-exec flag is set
- Signals set to be ignored in the calling process are set to be ignored in the new process.  Signals which are set to	be caught in the calling process image are set to default action in	the new	process image. Blocked signals remain blocked regardless of changes to the signal	action.
- The new process also inherits the following attributes from the calling process:
    - [process ID](https://www.freebsd.org/cgi/man.cgi?query=getpid&sektion=2&apropos=0&manpath=FreeBSD+12.0-RELEASE+and+Ports)
    - [parent process ID](https://www.freebsd.org/cgi/man.cgi?query=getppid&sektion=2&apropos=0&manpath=FreeBSD+12.0-RELEASE+and+Ports)
    - [process group ID](https://www.freebsd.org/cgi/man.cgi?query=getpgrp&sektion=2&apropos=0&manpath=FreeBSD+12.0-RELEASE+and+Ports)
    - [access groups](https://www.freebsd.org/cgi/man.cgi?query=getgroups&sektion=2&apropos=0&manpath=FreeBSD+12.0-RELEASE+and+Ports)
    - [working directory](https://www.freebsd.org/cgi/man.cgi?query=chdir&sektion=2&apropos=0&manpath=FreeBSD+12.0-RELEASE+and+Ports)
    - [root	directory](https://www.freebsd.org/cgi/man.cgi?query=chroot&sektion=2&apropos=0&manpath=FreeBSD+12.0-RELEASE+and+Ports)
    - [control terminal](https://www.freebsd.org/cgi/man.cgi?query=termios&sektion=4&apropos=0&manpath=FreeBSD+12.0-RELEASE+and+Ports)
    - [resource usages](https://www.freebsd.org/cgi/man.cgi?query=getrusage&sektion=2&apropos=0&manpath=FreeBSD+12.0-RELEASE+and+Ports)
    - [interval timers](https://www.freebsd.org/cgi/man.cgi?query=getitimer&sektion=2&apropos=0&manpath=FreeBSD+12.0-RELEASE+and+Ports)
    - [resource limits](https://www.freebsd.org/cgi/man.cgi?query=getrlimit&sektion=2&apropos=0&manpath=FreeBSD+12.0-RELEASE+and+Ports)
    - [file	mode mask](https://www.freebsd.org/cgi/man.cgi?query=umask&sektion=2&apropos=0&manpath=FreeBSD+12.0-RELEASE+and+Ports)
    - [signal mask](https://www.freebsd.org/cgi/man.cgi?query=sigaction&sektion=2&apropos=0&manpath=FreeBSD+12.0-RELEASE+and+Ports)

### Znajdź w katalogu `/bin` lub `/usr/bin` pliki wykonywalne z ustawionym *bitem uprawnień* `set-uid` lub `set-gid`. Do czego służą te bity?

**bit uprawnień** - bit oznaczający jakie uprawnienia (odczyt/zapisywanie/wykonywanie) ma właściciel pliku/członkowie grupy/inni użytkownicy. Bity uprawnień mają format `tuuugggooo` (t – typ pliku, u - właściciel, g – grupa, o - pozostali) gdzie w każdej z trzech kategorii znajdują się trzy bity `rwx` (read/write/execute).

Polecenie:
```bash
ls -al /usr/bin | grep "[r|\-][w|\-]s"
```

`set-uid`/`set-gid` - bit oznaczający, że użytkownik uruchamiający program uruchamia go z uprawnieniami jego właściciela/grupy właściciela (np. `sudo`).

# Zadanie 2

### Wywołanie [open(2)](https://www.freebsd.org/cgi/man.cgi?query=open&sektion=2) zawiedzie z błędem `ETXTBSY`, jeśli próbujemy otworzyć plik wykonywalny do zapisu pod warunkiem, że tenże plik jest w chwili obecnej wykonywany. Co mogłoby się stać, gdyby system operacyjny pozwolił modyfikować plik wykonywalny, który jest uruchomiony?

Jeżeli system używa stronicowania na żądanie, to wówczas kopiuje stronę z dysku do pamięci wtedy, gdy następuje próba dostępu do niej. Gdyby system pozwalał modyfikować uruchomione pliki wykonywalne, to wówczas istnieje możliwość, że dokonałby modyfikacji fragmentu tego pliku, który znajdowałby się na stronie niezaładowanej jeszcze do pamięci przez co przy próbie wykonania tego fragmentu mógłby zostać wykonany zmodyfikowany kod.

# Zadanie 3

### Które z operacji wymienionych w *§39* działają na katalogach?

(może trochę więcej oprócz tych)
- **mkdir**
- **rmdir**
- **closedir**
- **opendir**
- **readdir**
- **chmod**

### Na podstawie *§10.6.3* (rysunek10-32) przedstaw reprezentację katalogu i pokaż jak przebiega operacja usuwania pliku.

![zad](zad3.png)

**Reprezentacja katalogu**:
Katalog (w *ext2*) pozwala na nazwy plików do 255 znaków. Każdy katalog składa się z całkowitej liczby bloków co pozwala na jego atomowy zapis na dysku. Wewnątrz katalogu znajdują się nieposortowane wpisy plików i katalogów. Jeżeli któryś nie zajmuje całego bloku to wówczas bajty na jego końcu są nieużywane. 
Każdy wpis w katalogu składa się z czterech pół stałej długości i jednego o zmiennej. Pierwsze pole oznacza numer *inode* pliku. Następny, `rec_len` oznacza jak duży jest wpis (w bajtach, włączając ewentualny *padding* na końcu). Pole to jest potrzebne do znajdowania kolejnego wpisu. Następne pole oznacza typ pliku (plik, katalog itd.). Ostatnie pole o stałym rozmiarze mówi o tym jak długa jest nazwa pliku. Nazwa pliku jest *null terminated* i wyrównana do 32 bitów.

**Usuwanie pliku**:

Usuwanie pliku polega na zwiększeniu rozmiaru wpisu poprzedzającego usuwany wpis. Miejsce zajmowane przez usuwany wpis jest traktowane jako nieużywana część poprzedniego wpisu.

### Wpis katalogu nie zawiera *metadanych* pliku – gdzie w takim razie są one składowane?

Metadane składowane są w *inode*'ach.

**metadane** (pliku) - lista atrybutów pliku zawierająca informacje o nim. Np. na linuxie:
```c
/* Metadata returned by the stat and fstat functions */
struct stat 
{
    dev_t st_dev; /* Device */
    ino_t st_ino; /* inode */
    mode_t st_mode; /* Protection and file type */
    nlink_t st_nlink; /* Number of hard links */
    uid_t st_uid; /* User ID of owner */
    gid_t st_gid; /* Group ID of owner */
    dev_t st_rdev; /* Device type (if inode device) */
    off_t st_size; /* Total size, in bytes */
    unsigned long st_blksize; /* Blocksize for filesystem I/O */
    unsigned long st_blocks; /* Number of blocks allocated */
    time_t st_atime; /* Time of last access */
    time_t st_mtime; /* Time of last modification */
    time_t st_ctime; /* Time of last change */
};
```

###  Co wyraża pole `st_nlink` struktury `statbuf` opisanej w [stat(2)](http://man7.org/linux/man-pages/man2/stat.2.html)?

`st_nlink` oznacza liczbę dowiązań twardych (ang. *hard link*) do pliku. Licznik ten zlicza ile katalogów ma wpis dla tego pliku. Linki symboliczne nie wliczają się do tej sumy. *If the count is ever decremented to zero, then the file itself is discarded as soon as no process still holds it open.*

### Kiedy plik zostaje faktycznie usunięty z dysku?

Plik zostaje faktycznie usunięty z dysku kiedy licznik `st_nlink` będzie miał wartość zero (czyli nie istnieje żaden link do tego pliku).
