const passport = require('passport');
const { Strategy: LocalStrategy } = require('passport-local');
const { Strategy: BearerStrategy } = require('passport-http-bearer');
const { User } = require('../db/schema');
const { errorHandler } = require('../db/errors');

passport.use(
  new LocalStrategy({
    usernameField: 'username',
    passwordField: 'password'
  },
  function (username, password, done) {
    User
      .query()
      .where('username', username)
      .first()
      .then(function (user) {
        if (!user) { return done('Unknown user'); }
        user.verifyPassword(password, function (err, passwordCorrect) {
          if (err) { return done(err); }
          if (!passwordCorrect) { return done('Invalid password'); }
          return done(null, user)
        })
      }).catch(function (err) {
        done(err)
      })
  }
));

passport.use(new BearerStrategy(
  function(token, done) {
    User
      .query()
      .where('token', token)
      .first()
      .then(function (user){
        if (!user) { return done('Invalid Token'); }
        return done(null, user);
      }).catch(function (err) {
        done(err);
      });
  }
));

module.exports = passport;
