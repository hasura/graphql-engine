/**
 * Module dependencies.
 */
require('dotenv').config();

const express = require('express');
const bodyParser = require('body-parser');
const chalk = require('chalk');
const dotenv = require('dotenv');
const passport = require('passport');
const expressValidator = require('express-validator');
const cors = require('cors');

/**
 * Load environment variables from .env file, where API keys and passwords are configured.
 */
dotenv.load({ path: '.env' });

if(!process.env.ENCRYPTION_KEY) {
  throw new Error('JWT encryption key required')
}

/**
 * Create Express server.
 */
const app = express();
/**
 * Express configuration.
 */
app.use(cors());
app.set('host', '0.0.0.0');
app.set('port', process.env.PORT || 8080);
app.set('json spaces', 2); // number of spaces for indentation
app.use(bodyParser.json());
app.use(expressValidator());
app.use(passport.initialize());
app.use(passport.session());

const userController = require('./controllers/user');

app.post('/login', userController.postLogin);
app.post('/signup', userController.postSignup);
/**
 * Start Express server.
 */
app.listen(app.get('port'), () => {
  console.log('%s App is running at http://localhost:%d in %s mode', chalk.green('✓'), app.get('port'), app.get('env'));
  console.log('  Press CTRL-C to stop\n');
});

module.exports = app;
