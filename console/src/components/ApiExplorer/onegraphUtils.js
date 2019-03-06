import GraphiQLExplorer from 'graphiql-explorer-hasura';

export const makeDefaultArg = () => {
  return false;
};

export const getDefaultScalarArgValue = (parentField, arg, argType) => {
  return GraphiQLExplorer.defaultValue(argType);
};
