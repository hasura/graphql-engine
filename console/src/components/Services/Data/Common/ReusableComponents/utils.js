import React from 'react';

export const getUkeyPkeyConfig = columns => {
  const colLength = columns.length;
  if (!colLength) return null;
  if (colLength === 1) {
    return columns[0];
  }
  return `( ${columns.join(', ')} )`;
};

export const getForeignKeyConfig = (foreignKey, orderedColumns) => {
  const { refTableName, colMappings } = foreignKey;
  const filteredColMap = {};

  colMappings
    .filter(colMap => colMap.column !== '' && colMap.refColumn !== '')
    .forEach(colMap => {
      if (!orderedColumns[colMap.column]) return;
      filteredColMap[orderedColumns[colMap.column].name] = colMap.refColumn;
    });

  if (!refTableName || Object.keys(filteredColMap).length === 0) {
    return '';
  }

  const _lCol = Object.keys(filteredColMap);
  const _rCol = Object.values(filteredColMap);
  const lCol = _lCol.length > 1 ? '( ' + _lCol.join(', ') + ' )' : _lCol[0];
  const rCol = _rCol.length > 1 ? '( ' + _rCol.join(', ') + ' )' : _rCol[0];

  return `${lCol} → ${refTableName} . ${rCol}`;
};

export const pgConfTypes = {
  a: 'no action',
  r: 'restrict',
  c: 'cascade',
  n: 'set null',
  d: 'set default',
};

export const getExistingFKConstraints = (tableSchema, orderedColumns) => {
  return tableSchema.foreign_key_constraints.map(fkc => {
    const fk = {};
    fk.refTableName = fkc.ref_table;
    fk.refSchemaName = fkc.ref_table_table_schema;
    fk.onUpdate = pgConfTypes[fkc.on_update];
    fk.onDelete = pgConfTypes[fkc.on_delete];
    fk.constraintName = fkc.constraint_name;
    fk.colMappings = Object.keys(fkc.column_mapping).map(lc => ({
      column: orderedColumns.find(oc => oc.name === lc).index.toString(),
      refColumn: fkc.column_mapping[lc],
    }));
    fk.colMappings.push({ column: '', refColumn: '' });
    return fk;
  });
};

export const generateFKConstraintName = (
  tableName,
  lCols,
  existingConstraints,
  ignoreConstraints = []
) => {
  const expectedName = `${tableName}_${lCols
    .map(lc => lc.replace(/"/g, ''))
    .join('_')}_fkey`.substring(0, 60);

  let maxSuffix;
  for (let i = existingConstraints.length - 1; i >= 0; i--) {
    const existingConstraintName = existingConstraints[i].constraint_name;

    if (ignoreConstraints.includes(existingConstraintName)) {
      continue;
    }

    if (existingConstraintName.startsWith(expectedName)) {
      let currSuffix;

      if (existingConstraintName === expectedName) {
        currSuffix = 1;
      } else {
        const prefixLength = expectedName.length;
        currSuffix = parseInt(existingConstraintName.slice(prefixLength), 10);
      }

      if (!isNaN(currSuffix) && (!maxSuffix || currSuffix >= maxSuffix)) {
        maxSuffix = currSuffix;
      }
    }
  }

  return maxSuffix === undefined
    ? expectedName
    : `${expectedName}${maxSuffix + 1}`;
};

export const getUniqueConstraintName = (tableName, columns) => {
  return `${tableName}_${columns.join('_')}_key`;
};

export const getKeyDef = (config, constraintName) => {
  if (!config) {
    return null;
  }
  if (constraintName) {
    return (
      <div>
        <b>{config}</b> - <i>{constraintName}</i>
      </div>
    );
  }
  return (
    <div>
      <b>{config}</b>
    </div>
  );
};
