import http from 'http'
import express from 'express'
import multer from 'multer'

var app = express()
var upload = multer({ dest: 'uploads/' })

app.set('view engine', 'ejs')
app.set('views', './views')

app.get('/', (req, res) => {
    res.render('index')
})

app.post('/upload', upload.single('someFile'), (req, res) => {
    console.log(req.file)
})

http.createServer(app).listen(3000)