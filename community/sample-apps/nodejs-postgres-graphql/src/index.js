const Express = require("express");
const BodyParser = require("body-parser");
const Apis = require("./middleware/apis");
const Graphql = require("./middleware/graphql");

const App = Express();
const port = process.env.PORT;
const MwBodyParser = BodyParser.json();

App.use(MwBodyParser);
App.use(Apis);
App.use(Graphql);

App.listen(port, () => {
  console.log(`🚀 Server running on port ${port}.`);
});
