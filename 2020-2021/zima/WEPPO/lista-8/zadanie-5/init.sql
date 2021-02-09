CREATE TABLE Miejsce_Pracy (
    id    SERIAL PRIMARY KEY,
    nazwa VARCHAR NOT NULL
);

CREATE TABLE Osoba (
    id       SERIAL PRIMARY KEY,
    imie     VARCHAR NOT NULL,
    nazwisko VARCHAR NOT NULL,
    plec     CHAR(1),
    pesel    CHAR(11) NOT NULL
);

-- Relację wiele-do-wielu możemy modelować poprzez dodanie kolejnej tabeli, która "łączy" dwie inne
CREATE TABLE Miejsca_Pracy_Osoby (
    id_osoby INTEGER NOT NULL REFERENCES Osoba (id),
    id_pracy INTEGER NOT NULL REFERENCES Miejsce_Pracy (id)
);

GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO app;
GRANT USAGE, SELECT ON SEQUENCE osoba_id_seq TO app;
GRANT USAGE, SELECT ON SEQUENCE miejsce_pracy_id_seq TO app;
