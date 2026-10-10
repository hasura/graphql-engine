export const isBigQueryTable = (tableType: string) => {
  return tableType === 'TABLE' || tableType === 'BASE TABLE';
};
