const PORT = process.env.PORT || 4000;

const INIT_MESSAGE = `Scaffolder listening at ${PORT}`;

const SCAFFOLDS_REPO = process.env.SCAFFOLDS_REPO || "hasura/scaffolds";
const SCAFFOLDS_REPO_BRANCH = process.env.SCAFFOLDS_REPO_BRANCH || "master";

module.exports = {
  PORT,
  INIT_MESSAGE,
  SCAFFOLDS_REPO,
  SCAFFOLDS_REPO_BRANCH
};
