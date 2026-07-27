const passport = require('../config/passport')
const { User } = require('../db/schema')
const { errorHandler } = require('../db/errors')
const rasha = require('rasha')
const jwtConfig = require('../config/jwt')
const { check, validationResult } = require('express-validator')

/**
 * Sends the JWT key set
 */
exports.getJwks = async (req, res, next) => {
  const jwk = {
    ...rasha.importSync({ pem: jwtConfig.publicKey }),
    alg: 'RS256',
    use: 'sig',
    kid: jwtConfig.publicKey
  }
  const jwks = {
    keys: [jwk]
  }
  res.setHeader('Content-Type', 'application/json')
  res.send(JSON.stringify(jwks, null, 2) + '\n')
}

exports.loginValidators = [
  check('username', 'Username is not valid').notEmpty(),
  check('password', 'Password cannot be blank').notEmpty()
]

exports.signupValidators = [
  check('username', 'Username is not valid').notEmpty(),
  check('password', 'Password must be at least 4 characters long').isLength({
    min: 4
  }),
  check('confirmPassword', 'Passwords do not match').custom(
    (value, { req }) => value === req.body.password
  )
]

/**
 * Sign in using username and password and returns JWT
 */
exports.postLogin = async (req, res, next) => {
  const errors = validationResult(req)

  if (!errors.isEmpty()) {
    return res.status(400).json({ errors: errors.array() })
  }

  passport.authenticate('local', (err, user, info) => {
    if (err) {
      return handleResponse(res, 400, { error: err })
    }
    if (user) {
      handleResponse(res, 200, user.getUser())
    }
  })(req, res, next)
}

/**
 * POST /signup
 * Create a new local account
 */
exports.postSignup = async (req, res, next) => {
  const errors = validationResult(req)

  if (!errors.isEmpty()) {
    return res.status(400).json({ errors: errors.array() })
  }

  try {
    const user = await User.query()
      .allowGraph('[username, password]')
      .insert({
        username: req.body.username,
        password: req.body.password
      })
  } catch (err) {
    errorHandler(err, res)
    return
  }
  passport.authenticate('local', (err, user, info) => {
    if (err) {
      return handleResponse(res, 400, { error: err })
    }
    if (user) {
      handleResponse(res, 200, user.getUser())
    }
  })(req, res, next)
}

exports.getWebhook = async (req, res, next) => {
  passport.authenticate('bearer', (err, user, info) => {
    if (err) {
      return handleResponse(res, 401, { error: err })
    }
    if (user) {
      handleResponse(res, 200, user.getHasuraClaims())
    } else {
      handleResponse(res, 200, { 'X-Hasura-Role': 'anonymous' })
    }
  })(req, res, next)
}

function handleResponse(res, code, statusMsg) {
  res.status(code).json(statusMsg)
}
