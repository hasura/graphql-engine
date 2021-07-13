export const isSchemaSharingEnabled = false;

const repo_owner = 'hasura';
const repo_name = 'schema-sharing';
const repo_branch = 'main';

export const BASE_URL_TEMPLATE = `https://raw.githubusercontent.com/${repo_owner}/${repo_name}/${repo_branch}`;
export const BASE_URL_PUBLIC = `https://github.com/${repo_owner}/${repo_name}/blob/${repo_branch}`;
export const ROOT_CONFIG_PATH = `${BASE_URL_TEMPLATE}/config.json`;
