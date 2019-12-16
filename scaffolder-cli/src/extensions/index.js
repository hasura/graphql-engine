const jsonBodyParser = require('./bodyParser');

const useExtensions = (app) => {
  app.use(jsonBodyParser);
};

module.exports = useExtensions;