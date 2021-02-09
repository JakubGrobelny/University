import pg from 'pg'

const client = new pg.Client({
    host: 'localhost',
    database: 'weppo',
    user: 'app',
    password: 'abc123',
    port: 5432
})

async function measureAsyncFunctionTime(f, label) {
    console.time(label)
    await f()
    console.timeEnd(label)
}

async function findPeopleBySurname(surname) {
    await client.connect()
    try {
        let result = await client.query('SELECT * FROM Osoba WHERE nazwisko=$1', [surname])
        console.log('Found ' + result.rowCount + ' rows.')
    } catch (err) {
        console.error(err)
    }
    client.end()
}

measureAsyncFunctionTime(async () => findPeopleBySurname('Kowalski'))

// Bez indeksu:
// Found 49960 rows.
// default: 149.187ms

// Z indeksem (index.sql)
// Found 49960 rows.
// default: 121.769ms