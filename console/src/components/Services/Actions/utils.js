export const findType = (types, typeName) => {
  return types.find(t => t.name === typeName);
};

export const getActionName = action => {
  return action.action_name;
};

export const findAction = (actions, actionName) => {
  return actions.find(a => getActionName(a) === actionName);
};

export const getActionOutputType = action => {
  return action.action_defn.output_type;
};

export const getActionOutputFields = (action, types) => {
  const outputTypeName = getActionOutputType(action);

  const outputType = findType(types, outputTypeName);

  return outputType.fields;
};

export const getActionArguments = action => {
  return action.action_defn.arguments;
};

export const getActionType = action => {
  return action.action_defn.type
};

export const getAllActions = getState => {
  return getState().actions.common.actions;
};

export const getActionPermissions = action => {
  return action.permissions;
};

export const findActionPermission = (perms, role) => {
  return perms.find(p => p.role_name === role);
};

export const getActionComment = action => action.comment;
