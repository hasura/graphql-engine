const { SCAFFOLDS_REPO, SCAFFOLDS_REPO_BRANCH } = require("../constants");

const getTemplatePath = (framework) => {
  return `https://raw.githubusercontent.com/hasura/graphql-engine/master/community/action-scaffolds/${framework}.js`
}

module.exports.getTemplatePath = getTemplatePath;
