const casbin = require("casbin");
const path = require("path");

const login = async (resolve, root, args, context, info) => {
  // authenticate the user and set the idToken in the context
  const { idToken } = await authenticate(args.email, args.password);
  context.auth = { idToken };
  return resolve(root, args, context, info);
};

const logout = async (resolve, root, args, context, info) => {
  // clear the idToken from the context
  context.auth = {};
  return resolve(root, args, context, info);
};

const auth = async (resolve, root, args, context, info) => {
  // enforce policies using casbin
  const { idToken } = context.auth;
  const enforcer = await casbin.newEnforcer(path.resolve(__dirname, "../config/model.conf"));
  if (await enforcer.enforce(idToken.sub, context.operation, context.resource)) {
    return resolve(root, args, context, info);
  }
  throw new Error("Forbidden");
};

module.exports = {
  login,
  logout,
  auth,
};