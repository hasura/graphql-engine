/* eslint-disable no-console */
const express = require("express");
const { join } = require("path");
const morgan = require("morgan");
const app = express();

app.use(morgan("dev"));
app.use(express.static(join(__dirname, "dist")));

app.use((_, res) => {
  res.sendFile(join(__dirname, "dist", "index.html"));
});

app.listen(8080, () => console.log("Listening on port 8080"));
