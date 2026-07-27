/**
 * Module dependencies.
 */

const express = require('express')
const bodyParser = require('body-parser')
// const dotenv = require('dotenv');
const passport = require('passport')
const cors = require('cors')

/**
 * Load environment variables from .env file, where API keys and passwords are configured.
 */
// dotenv.load({ path: '.env.example' });

/**
 * Controllers (route handlers).
 */
const userController = require('./controllers/user')

const app = express()

/**
 * Express configuration.
 */
app.set('host', '0.0.0.0')
app.set('port', process.env.PORT || 8080)
app.set('json spaces', 2) // number of spaces for indentation
app.use(cors())
app.use(bodyParser.json())
app.use(passport.initialize())

app.post('/login', userController.loginValidators, userController.postLogin)
app.post(
  '/signup',
  userController.signupValidators,
  userController.postSignup
)
app.get('/webhook', userController.getWebhook)
app.get('/jwks', userController.getJwks)
/**
 * Start Express server.
 */
app.listen(app.get('port'), () => {
  console.log(
    '✓ App is running at http://localhost:%d in %s mode',
    app.get('port'),
    app.get('env')
  )
})

module.exports = app
