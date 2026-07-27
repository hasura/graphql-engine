const passport = require('../config/passport');
const { User } = require('../db/schema');
const { errorHandler } = require('../db/errors');
const { body, validationResult } = require('express-validator');

/**
 * POST /login
 * Sign in using username and password.
 */
exports.postLogin = [
  body('username', 'Username is not valid').notEmpty(),
  body('password', 'Password cannot be blank').notEmpty(),
  async (req, res, next) => {
    const errors = validationResult(req);

    if (!errors.isEmpty()) {
      return res.status(400).json({'errors': errors.array()});
    }

    passport.authenticate('local', (err, user, info) => {
      if (err) { return handleResponse(res, 400, {'error': err})}
      if (user) {
        handleResponse(res, 200, user.getUser());
      }
    })(req, res, next);
  }
];


/**
 * POST /signup
 * Create a new local account.
 */
exports.postSignup = [
  body('username', 'Username is not valid').notEmpty(),
  body('password', 'Password must be at least 4 characters long').isLength({ min: 4 }),
  body('confirmPassword', 'Passwords do not match').custom((value, { req }) => value === req.body.password),
  async (req, res, next) => {
    const errors = validationResult(req);

    if (!errors.isEmpty()) {
      return res.status(400).json({'errors': errors.array()});
    }

    try {
      await User.query()
        .insert({
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
  }
];

exports.getWebhook = async (req, res, next) => {
  passport.authenticate('bearer', (err, user, info) => {
    if (err) { return handleResponse(res, 401, {'error': err}); }
    if (user) {
      handleResponse(res, 200, {
        'X-Hasura-Role': 'user',
        'X-Hasura-User-Id': `${user.id}`
      });
    } else {
      handleResponse(res, 200, {'X-Hasura-Role': 'anonymous'});
    }
  })(req, res, next);
}


function handleResponse(res, code, statusMsg) {
  res.status(code).json(statusMsg);
}
