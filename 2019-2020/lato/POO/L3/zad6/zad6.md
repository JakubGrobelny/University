- **Wytłumaczyć różnicę między SRP a ISP.**

SRP oznacza, że żadna klasa nie może posiadać więcej niż jednej odpowiedzialności, czyli powodu do modyfikacji. Komponenty całego programu powinny więc jak najmniejsze, skupione na jednej funkcjonalności i posiadające jak największą spójność. Ułatwia to utrzymywanie takiego kodu i wprowadzania w nim zmian.

ISP oznacza, że interfejs powinien być zaprojektowany w taki sposób, że nie wymaga od klienta używania tych jego fragmentów, które nie są mu potrzebne. Interfejs nie powinien  wymagać od klienta implementowania zbędnych mu metod tylko po to, żeby mógł skorzystać z części funkcjonalności. Poszczególne interfejsy powinny być minimalne i skupione na jednym zadaniu.

Idee stojące za SRP i ISP są bardzo podobne, ale SRP skupia się bardziej na perspektywie projektanta systemu, a ISP na perspektywie korzystającego z kodu klienta. Możliwe jest, że kod łamie zasady ISP poprzez przytłoczenie klienta niepotrzebnymi mu feature'ami ale jednocześnie przestrzega SRP.