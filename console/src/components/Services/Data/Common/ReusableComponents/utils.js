export const getForeignKeyConfig = (foreignKey, orderedColumns) => {
  const { refTableName, colMappings } = foreignKey;
  const filteredColMap = {};
  colMappings
    .filter(colMap => colMap.column !== '' && colMap.refColumn !== '')
    .forEach(
      colMap => {
        if (!orderedColumns[colMap.column]) return;
        filteredColMap[orderedColumns[colMap.column].name] = colMap.refColumn
      }
    );
  if (!refTableName || Object.keys(filteredColMap).length === 0) {
    return '';
  }
  return `( ${Object.keys(filteredColMap).join(
    ', '
  )} ) → ${refTableName} ( ${Object.values(filteredColMap).join(', ')} )`;
};

export const pgConfTypes = {
  'a': 'no action',
  'r': 'restrict',
  'c': 'cascade',
  'n': 'null',
  'd': 'set default'
};

export const getExistingFKConstraints = (tableSchema, orderedColumns) => {
  return tableSchema.foreign_key_constraints.map((fkc) => {
    const fk = {};
    fk.refTableName = fkc.ref_table;
    fk.onUpdate = pgConfTypes[fkc.on_update];
    fk.onDelete = pgConfTypes[fkc.on_delete];
    fk.constraintName = fkc.constraint_name;
    fk.colMappings = Object.keys(fkc.column_mapping).map((lc) => ({
      column: orderedColumns.find(oc => oc.name === lc).index.toString(),
      refColumn: fkc.column_mapping[lc]
    }));
    fk.colMappings.push({'': ''});
    return fk;
  });
}