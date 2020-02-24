const { GRAPHQL_ENGINE_REPO_BRANCH, GRAPHQL_ENGINE_REPO_OWNER } = require("../constants");

const getTemplatePath = (framework) => {
  return `https://raw.githubusercontent.com/hasura/codegen-assets/master/${framework}/actions-codegen.js`
}

module.exports.getTemplatePath = getTemplatePath;
