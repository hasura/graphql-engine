import { getCurrTimeForFileName } from '../../../../components/Common/utils/export.utils';

const replaceAllDotsWithUnderscore = (text: string) => text.replace(/\./g, '_');

export const getFileName = (tableName: string) => {
  const replacedTableName = replaceAllDotsWithUnderscore(tableName);
  const currentTime = getCurrTimeForFileName();
  return `export_${replacedTableName}_${currentTime}`;
};
