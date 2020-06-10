const PORT = process.env.PORT || 4000;

const GRAPHQL_ENGINE_REPO_OWNER = process.env.GRAPHQL_ENGINE_REPO_OWNER || 'hasura';
const GRAPHQL_ENGINE_REPO_BRANCH = process.env.GRAPHQL_ENGINE_REPO_BRANCH || 'master';

module.exports = {
  PORT,
  GRAPHQL_ENGINE_REPO_OWNER,
  GRAPHQL_ENGINE_REPO_BRANCH,
};
