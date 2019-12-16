const bodyParser = require('body-parser');

module.exports = bodyParser.json({ extended: true, limit: '10mb'});