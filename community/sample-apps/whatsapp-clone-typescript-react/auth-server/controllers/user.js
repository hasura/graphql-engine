const passport = require('../config/passport');
const { User } = require('../db/schema');
const { errorHandler } = require('../db/errors');
const jwt = require('jsonwebtoken');

/**
 * POST /login
 * Sign in using username and password.
 */
exports.postLogin = async (req, res, next) => {
  req.assert('username', 'Username is not valid').notEmpty();
  req.assert('password', 'Password cannot be blank').notEmpty();

  const errors = req.validationErrors();

  if (errors) {
    return res.status(400).json({'errors': errors});
  }

  passport.authenticate('local', (err, user) => {
    if (err) { return handleResponse(res, 400, {'error': err})}
    if (user) {

      const tokenContents = {
        sub: '' + user.id,
        name: user.username,
        iat: Date.now() / 1000,
        "https://hasura.io/jwt/claims": {
          "x-hasura-allowed-roles": ["mine","user"],
          "x-hasura-user-id": '' + user.id,
          "x-hasura-default-role": "user",
          "x-hasura-role": "user"
        }
      }

      handleResponse(res, 200, {
        token: jwt.sign(tokenContents, process.env.ENCRYPTION_KEY)
      });
    }
  })(req, res, next);
};


/**
 * POST /signup
 * Create a new local account.
 */
exports.postSignup = async (req, res, next) => {
  req.assert('name', 'Name is not valid').notEmpty();
  req.assert('username', 'Username is not valid').notEmpty();
  req.assert('password', 'Password must be at least 4 characters long').len(4);
  req.assert('confirmPassword', 'Passwords do not match').equals(req.body.password);

  const errors = req.validationErrors();

  if (errors) {
    return res.status(400).json({'errors': errors});
  }

  try {
    await User.query()
    .allowInsert('[name, username, password]')
    .insert({
      name: req.body.name,
      username: req.body.username,
      password: req.body.password
    });
  } catch (err) {
    errorHandler(err, res);
    return;
  }
  passport.authenticate('local', (err, user, info) => {
    if (err) {  return handleResponse(res, 400, {'error': err})}
    if (user) {
      handleResponse(res, 200, user.getUser());
    }
  })(req, res, next);
};


function handleResponse(res, code, statusMsg) {
  res.status(code).json(statusMsg);
}
