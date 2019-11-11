# Spis treści

- [Zadanie 1](#zadanie-1)
- [Zadanie 2](#zadanie-2)

***

# Zadanie 1

### Czym różnią się ścieżka *absolutna*, *relatywna* i *znormalizowan*a?

- **ścieżka absolutna** – wskazuje na ten sam plik niezależnie od aktualnego katalogu roboczego (zaczyna się katalogiem *root*a).
- **ścieżka relatywna** – wskazuje na lokalizację względem aktualnego katalogu roboczego.
- **ścieżka znormalizowana** – (*??? jedyne co znalazłem to [coś](https://blogs.msdn.microsoft.com/jeremykuhne/2016/04/21/path-normalization/) na (tfu) Windowsie*) ścieżka bez `.` , `..` + bez końcowych białych znaków.

### Względem którego katalogu obliczana jest ścieżka relatywna? Jakim wywołaniem systemowym zmienić ten katalog?

Ścieżka relatywna obliczana jest względem aktualnego katalogu roboczego (*current working directory*).

Można zmienić ten katalog wywołąniem [`chdir(2)`](https://linux.die.net/man/2/chdir).

### Wyjaśnij czym są *punkty montażowe*, a następnie na podstawie [mount(8)](https://www.freebsd.org/cgi/man.cgi?mount(8)) wyjaśnij znaczenie i zastosowanie następujących atrybutów punktów montażowych: `noatime`, `noexec` i `sync`.

**punkt montażowy** (ang. *mount point*) – katalog w systemie plików do którego logicznie dołączony (*logically attached*) jest dodatkowy system plików (np. z innego urządzenia). Punkt montażowy staje się *root directory* nowo dodanego systemu plików i umożliwia do niego dostęp.


- `noatime` – powoduje, że czasy dostępów do *inode*'ów nie są aktualizowane w danym systemie plików (na przykład w celu zwiększenia szybkości dostępu gdy plików jest dużo).
- `noexec` – zakaz bezpośredniego wykonywania plików binarnych ze wskazanego zamontowanego systemu plików (nie dotyczy plików tekstowych ze skryptami z *shebang*iem). Ta opcja jest przydatna dla serwerów, które mają systemy plików zawierające pliki wykonywalne dla architektur innych niż własna.
- `sync` – powoduje, że wszelkie operacje I/O do systemu plików muszą zostać wykonane synchronicznie. Może skrócić życie urządzeń z ograniczoną liczbą zapisów ale zapewnia spójność zapisywanych danych.

***
