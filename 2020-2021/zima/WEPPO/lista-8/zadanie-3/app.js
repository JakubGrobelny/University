import pg from 'pg'

const client = new pg.Client({
    host: 'localhost',
    database: 'weppo',
    user: 'app',
    password: 'abc123',
    port: 5432
})

async function insertPerson() {
    client.query(
        `INSERT INTO Osoba (imie, nazwisko, pesel)
         VALUES ('Test', 'Person', '00011100000')
         RETURNING id`
    );
}

async function updatePerson() {
    client.query("UPDATE Osoba SET plec = '?' WHERE plec is NULL")
}

async function deletePerson() {
    client.query("DELETE FROM Osoba WHERE plec = '?'")
}

async function fetchPerson() {
    let queryResult = await client.query("SELECT * FROM Osoba WHERE plec = '?'")
    if (queryResult.rowCount == 0) {
        console.log("Test person was not found");
    } else {
        let name = queryResult.rows[0].imie + ' ' + queryResult.rows[0].nazwisko
        console.log("Found " + queryResult.rowCount + " results. The first one is " + name)
    }
}

(async function () {
    await client.connect()

    try {
        await fetchPerson()

        await insertPerson()
        await fetchPerson()

        await updatePerson()
        await fetchPerson()

        await deletePerson()
        await fetchPerson()
    } catch (err) {
        console.log(err)
    }

    client.end()
})()
