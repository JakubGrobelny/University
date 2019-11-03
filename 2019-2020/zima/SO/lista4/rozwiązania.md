# Spis treści

- [Zadanie 1](#zadanie-1)

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