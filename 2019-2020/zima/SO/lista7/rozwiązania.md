# Spis treści

***

# Zadanie 1 

### Na podstawie §49.1 wyjaśnij słuchaczom różnicę między *odwzorowaniami plików w pamięć* (ang. memory-mapped files) i *odwzorowaniami pamięci anonimowej* (ang. anonymous mappings).

- **odwzorowanie pliku w pamięć** – zmapowanie obszaru pliku bezpośrednio do pamięci wirtualnej procesu. Umożliwia to bezpośredni dostęp do zawartości plików i operowanie na jego bajtach. Strony są automatycznie ładowane z pliku w razie potrzeby.

- **odwzorowanie pamieci anomimowej** – mapowanie, które nie ma odpowiadajacego mu pliku. 

### Jaką zawartością wypełniana jest pamięć wirtualną należącą do tychże odwzorowań?

- pliki – zawartość pliku z dysku
- pamięć anonimowa – zainicjalizowana zerami

### Czym różni się odwzorowanie *prywatne* od *dzielonego*?

Pamięć odwzorowania dla jednego procesu może być dzielona z odwzorowaniami w innych procesach (tj. w tabeli stron wpisy obu procesów wskazują na te same strony w pamięci RAM). Może to stać się na dwa sposoby: dwa procesy odwzorowują ten sam obszar pliku, lub poprzez `fork()`.

- **odwzorowanie prywatne** (`MAP_PRIVATE`) – modyfikacje zawartości odwzorowania nie są widoczne dla innych procesów, a dla odwzorowań plików nie są zapisywane do pliku. Zmiany w prywatnych odwzorowaniach są prywatne dla każdego procesu. Jądro dokonuje tego poprzez użycie **kopiowania przy zapisie**. Oznacza to, że kiedy proces próbuje modyfikować zawartość strony, jądro tworzy nową, oddzielną kopię tej strony dla procesu.
- **odwzorowanie dzielone** – modyfikacje zawartości odwzorowania są widoczne dla innych procesów dzielących to samo odwzorowanie oraz zapisywane do pliku w przypadku odwzorowań pliku.

### Dlaczego odwzorowania prywatne wykorzystują technikę *kopiowania przy zapisie*?

Odwzorowania prywatne wykorzystują technikę **kopiowanie przy zapisie** po to, żeby zmiany w odwzorowanej pamięci nie były widoczne dla innych procesów. Dopóki procesy nie dokonują żadnych zapisów, to nie ma potrzeby kopiowania pamięci. Gdy któryś spróbuje coś zapisać, powinno to być widoczne jedynie dla niego (dostaje on więc własną kopię).
