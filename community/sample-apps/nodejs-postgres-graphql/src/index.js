const express = require("express");
const bodyParser = require("body-parser");

const db = require("./queries");

const app = express();
const port = 8080;

app.use(bodyParser.json());
app.use(
  bodyParser.urlencoded({
    extended: true
  })
);
app.get("/", db.testDB);

app.listen(port, () => {
  console.log(`App running on port ${port}.`);
});
