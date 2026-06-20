const { login, logout, auth } = require("./authMiddleware.js");

module.exports = {
  Mutation: {
    login: (root, args, context, info) => login(() => {}, root, args, context, info),
    logout: (root, args, context, info) => logout(() => {}, root, args, context, info),
  },
  Query: {
    me: (root, args, context, info) => auth((root, args, context, info) => {
      const { idToken } = context.auth;
      return User.findById(idToken.sub);
    }, root, args, context, info),
  },
};