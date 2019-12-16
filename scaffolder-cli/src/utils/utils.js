const { SCAFFOLDS_REPO, SCAFFOLDS_REPO_BRANCH } = require("../constants");

const getTemplatePath = (framework) => {
  return `https://raw.githubusercontent.com/${SCAFFOLDS_REPO}/${SCAFFOLDS_REPO_BRANCH}/${framework}/service/.template.js`;
}

module.exports.getTemplatePath = getTemplatePath;
