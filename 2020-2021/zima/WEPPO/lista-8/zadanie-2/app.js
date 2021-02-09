import pg from 'pg'

// Zakładamy, że istnieje baza danych z poniższymi parametrami
const client = new pg.Client({
    host: 'localhost',
    database: 'weppo',
    user: 'app',
    password: 'abc123',
    port: 5432
})

// Pobieranie danych
async function fetchData() {
    let result = await client.query('SELECT * from Osoba')
    console.log(result.rows)
}

// Wstawianie danych
async function insertPerson(person) {
    let result = await client.query(
        `INSERT INTO Osoba (imie, nazwisko, plec, pesel)
        VALUES ($1, $2, $3, $4)
        RETURNING id`,
        [person.name, person.surname, person.gender, person.pesel])
    return result.rows[0].id
}

(async function () {
    await client.connect()

    let person = {
        name: "Jan",
        surname: "Nowak",
        gender: "M",
        pesel: "98062700000"
    }

    try {
        await fetchData()
        let id = await insertPerson(person)
        console.log('id of the new person is ' + id)
    } catch (err) {
        console.error(err)
    }
    client.end()
})()
