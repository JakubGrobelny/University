-- Imiona wszystkich osób.
SELECT imie FROM Osoba;

-- Wszystkie unikatowe imiona w tabeli.
SELECT DISTINCT imie FROM Osoba;

-- Wszystkie unikatowe pary (imię, nazwisko).
SELECT DISTINCT (imie, nazwisko) FROM Osoba;

-- Wszystkie osoby, które nie mają podanej płci.
SELECT * FROM Osoba WHERE plec IS NULL;

-- Wszystkie kobiety w tabeli.
SELECT (imie, nazwisko, pesel) FROM Osoba WHERE plec = 'K';

-- Wszystkie osoby, które mają na nazwisko Kowalski/Kowalska.
SELECT * FROM Osoba WHERE nazwisko SIMILAR TO 'Kowalsk(a|i)';

-- Wszystkie imiona z tabeli zaczynające się na 'A'.
SELECT * FROM Osoba WHERE like(imie, 'A%');

-- Wszystkie osoby mające wpisaną płeć 'K' lub imię kończące się na 'a'.
SELECT * FROM Osoba WHERE (plec = 'K') OR like(imie, '%a');

-- Wszystkie osoby, mające na nazwisko Nowak będące mężczyznami.
SELECT * FROM Osoba WHERE (nazwisko = 'Nowak') AND (plec = 'M');

-- Liczba osób z danym nazwiskiem.
SELECT nazwisko, COUNT(imie) FROM Osoba GROUP BY nazwisko;

-- Nazwiska, które ma więcej niż 1 osoba. Używając funkcji agregujących musimy
-- użyć HAVING zamiast WHERE.
SELECT nazwisko, COUNT(imie)
    FROM Osoba
    GROUP BY nazwisko
    HAVING COUNT(imie) > 1;