const express = require("express");
const bodyParser = require("body-parser");

const app = express();
const port = 8080;

app.use(bodyParser.json());
app.use(
  bodyParser.urlencoded({
    extended: true
  })
);
app.get("/", (request, response) => {
  response.json({ message: "Hello from express+postgres" });
});

app.listen(port, () => {
  console.log(`App running on port ${port}.`);
});
