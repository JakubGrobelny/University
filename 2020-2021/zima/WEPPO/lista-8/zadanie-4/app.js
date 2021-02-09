import pg from 'pg'

const client = new pg.Client({
    host: 'localhost',
    database: 'weppo',
    user: 'app',
    password: 'abc123',
    port: 5432
})

async function addWorkplace(workplaceName) {
    let result = await client.query(
        'INSERT INTO Miejsce_Pracy (nazwa) VALUES ($1) RETURNING id',
        [workplaceName]
    )
    return result.rows[0].id
}

async function addPerson(person) {
    await client.query(
        `INSERT INTO Osoba (imie, nazwisko, plec, pesel, praca)
         VALUES ($1, $2, $3, $4, $5)`,
        [person.name, person.surname, person.gender, person.pesel, person.workplace]
    )
}

async function addWorkplaceAndPerson(person, workplaceName) {
    let workplaceId = await addWorkplace(workplaceName)
    person.workplace = workplaceId
    await addPerson(person)
}

(async function () {
    let person = {
        name: 'Robert',
        surname: '); DROP TABLE Osoby;--',
        gender: 'M',
        pesel: '98000000001'
    }
    await client.connect()
    try {
        await addWorkplaceAndPerson(person, "Database Administrator")
    } catch (err) {
        console.error(err)
    }
    await client.end()
})()