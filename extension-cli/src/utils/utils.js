const { GRAPHQL_ENGINE_REPO_BRANCH, GRAPHQL_ENGINE_REPO_OWNER } = require("../constants");

const getTemplatePath = (framework) => {
  return `https://raw.githubusercontent.com/${GRAPHQL_ENGINE_REPO_OWNER}/graphql-engine/${GRAPHQL_ENGINE_REPO_BRANCH}/community/action-scaffolders/${framework}.js`
}

module.exports.getTemplatePath = getTemplatePath;
