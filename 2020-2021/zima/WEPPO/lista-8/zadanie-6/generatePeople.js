import pg from 'pg'
import fs from 'fs'

const client = new pg.Client({
    host: 'localhost',
    database: 'weppo',
    user: 'app',
    password: 'abc123',
    port: 5432
})

async function getLines(filePath) {
    let file = await fs.promises.readFile(filePath)
    return file.toString().split("\n").map((str) => str.trim())
}

Array.prototype.random = function () {
    return this[Math.floor((Math.random() * this.length))]
}

async function generatePeople(numberOfPeople) {
    const surnames = await getLines('./surnames.txt')
    const names = await getLines('./names.txt')
    await client.connect()

    try {
        for (let i = 0; i < numberOfPeople; i++) {
            let surname = surnames.random()
            let name = names.random()
            let pesel = Math.random().toString(10).slice(3, 14)
            await client.query(
                `INSERT INTO Osoba (imie, nazwisko, plec, pesel)
                 VALUES ($1, $2, $3, $4)`,
                 [name, surname, 'M', pesel]
            )
        }
    } catch (err) {
        console.error(err)
    }

    client.end()
}

generatePeople(5000000)