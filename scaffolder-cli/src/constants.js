const PORT = process.env.PORT || 4000;

const INIT_MESSAGE = `Scaffolder listening at ${PORT}`;

const GRAPHQL_ENGINE_REPO_OWNER = process.env.GRAPHQL_ENGINE_REPO_OWNER || 'hasura'
const GRAPHQL_ENGINE_REPO_BRANCH = process.env.GRAPHQL_ENGINE_REPO_BRANCH || 'master'

module.exports = {
  PORT,
  INIT_MESSAGE,
  GRAPHQL_ENGINE_REPO_OWNER,
  GRAPHQL_ENGINE_REPO_BRANCH 
};
