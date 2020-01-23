import { CODEGEN_REPO, ALL_FRAMEWORKS_FILE_PATH } from '../constants';

export const getCodegenFilePath = framework => {
  return `https://raw.githubusercontent.com/${CODEGEN_REPO}/master/${framework}/codegen.js`;
};

export const getAllCodegenFrameworks = () => {
  return fetch(ALL_FRAMEWORKS_FILE_PATH)
    .then(r => r.json())
    .catch(e => {
      console.error('could not fetch the latest codegen file');
      throw e;
    });
};
