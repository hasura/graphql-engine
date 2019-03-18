export const getForeignKeyConfig = (foreignKey, orderedColumns) => {
  const { refTableName, colMappings } = foreignKey;
  const filteredColMap = {};
  colMappings
    .filter(colMap => colMap.column !== '' && colMap.refColumn !== '')
    .forEach(
      colMap =>
        (filteredColMap[orderedColumns[colMap.column].name] = colMap.refColumn)
    );
  if (!refTableName || Object.keys(filteredColMap).length === 0) {
    return '';
  }
  return `( ${Object.keys(filteredColMap).join(
    ', '
  )} ) → ${refTableName} ( ${Object.values(filteredColMap).join(', ')} )`;
};
