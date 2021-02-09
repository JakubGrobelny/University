-- Tworzenie tabeli:

CREATE TABLE Osoba (
    -- Automatycznie generujacy się klucz
    id       SERIAL PRIMARY KEY,
    -- Imiona i nazwiska mogą być dowolnie długie, więc VARCAHAR
    imie     VARCHAR NOT NULL,
    nazwisko VARCHAR NOT NULL,
    -- 'K'/'M', wystarczy jeden znak
    plec     CHAR(1),
    -- PESEL ma zawsze 11 cyfr, więc można użyć CHAR(11)
    pesel    CHAR(11) NOT NULL
);

-- Dodawanie rekordów do tabeli:

INSERT INTO Osoba (imie, nazwisko, plec, pesel)
    VALUES ('Jan', 'Nowak', 'M', '00011122233');

GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO app;
GRANT USAGE, SELECT ON SEQUENCE osoba_id_seq TO app;