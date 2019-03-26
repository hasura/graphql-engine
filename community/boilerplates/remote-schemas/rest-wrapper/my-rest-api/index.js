const express = require('express');
const bodyParser = require("body-parser");
const uuidv4 = require('uuid/v4');
const app = express();
const port = process.env.PORT || 3000;

app.use(bodyParser.json());

var users = [];

app.get('/', (req, res) => res.json('Hello World!'));

app.get('/users/:userId', (req, res) => {
    var idParam = req.params.userId;
    var result = users.find(user => idParam ? user.id == idParam: false);
    if(result) {
        res.json(result);
    } else {
        res.status(404).json("user not found");
    }
});

app.get('/users', (req, res) => {
    var nameParam = req.query.name;
    var result = users.filter(user => nameParam ? user.name == nameParam : true);
    res.json(result);
});

app.post('/users', (req, res) => {
    var user = req.body;
    user.id = user.id ? user.id : uuidv4();

    if (user.id && user.name && user.balance) {
        if (user.balance >= 100) {
            users.push(user);
            res.json(user);
	      } else {
	          res.status(400).json('minimum balance required: 100');
	      }
    } else {
        res.status(400).json('invalid parameters');
    }
});

app.listen(port, () => console.log(`Example app listening on port ${port}!`));
