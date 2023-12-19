const express = require('express')
const app = express()
const port = 3050

app.use(express.json())

app.post('/validate-request', (req, res) => {
    let response = {};
    const headers = Object.fromEntries(Array.from(req.body["headers"]));
     for (const key in req.body["headers"]) {
         response[key] = String(req.body["headers"][key]);
    }
    res.status(200).json(response);
})

app.listen(port, () => {
    console.log(`Dev webhook authentication listening at http://localhost:${port}`)
}
)
