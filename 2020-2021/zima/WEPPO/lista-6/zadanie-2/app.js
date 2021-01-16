import http from 'http'
import express from 'express'

var app = express()

app.set('view engine', 'ejs')
app.set('views', './views')

app.get('/', (req, res) => {
    let checkboxes = {
        name: 'checkboxes',
        options: [
            { name: "first",  text: "Pierwsza opcja" },
            { name: "second", text: "Druga opcja" },
            { name: "third",  text: "Trzecia opcja" }
        ]
    }

    res.render('index', {checkboxes})
})

http.createServer(app).listen(3000)