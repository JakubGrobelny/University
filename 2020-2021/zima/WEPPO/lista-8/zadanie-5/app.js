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
    let result = await client.query(
        `INSERT INTO Osoba (imie, nazwisko, plec, pesel)
         VALUES ($1, $2, $3, $4)
         RETURNING id`,
        [person.name, person.surname, person.gender, person.pesel]
    )
    return result.rows[0].id
}

async function assignWorkplaceById(personId, workplaceId) {
    await client.query(
        `INSERT INTO Miejsca_Pracy_Osoby (id_osoby, id_pracy)
         VALUES ($1, $2)`,
        [personId, workplaceId]
    )
}

(async function () {
    await client.connect()
    try {
        let people = [
            {
                name: 'Robert',
                surname: '); DROP TABLE Osoby;--',
                gender: 'M',
                pesel: '98000000001'
            },
            {
                name: 'Jan',
                surname: 'Nowak',
                gender: 'M',
                pesel: '12312312312'
            }
        ]
        let personIds = await Promise.all(people.map(async (person) => {
            return await addPerson(person)
        }))

        let workplaces = ['Database Administrator', 'Software Developer']
        let workplaceIds = await Promise.all(workplaces.map(async (workplace) => {
            return await addWorkplace(workplace)
        }))

        for (const personId of personIds) {
            for (const workplaceId of workplaceIds) {
                await assignWorkplaceById(personId, workplaceId)
            }
        }
    } catch (err) {
        console.error(err)
    }
    client.end()
})()
