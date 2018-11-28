const ordinalColSort = (a, b) => {
  if (a.ordinal_position < b.ordinal_position) {
    return -1;
  }
  if (a.ordinal_position > b.ordinal_position) {
    return 1;
  }
  return 0;
};

const findFKConstraint = (curTable, column) => {
  const fkConstraints = curTable.foreign_key_constraints;

  return fkConstraints.find(
    fk =>
      Object.keys(fk.column_mapping).length === 1 &&
      Object.keys(fk.column_mapping)[0] === column
  );
};

const findTableFromRel = (schemas, curTable, rel) => {
  let rtable = null;

  // for view
  if (rel.rel_def.manual_configuration !== undefined) {
    rtable = rel.rel_def.manual_configuration.remote_table;
    if (rtable.schema) {
      rtable = rtable.name;
    }
  }

  // for table
  if (rel.rel_def.foreign_key_constraint_on !== undefined) {
    // for object relationship
    if (rel.rel_type === 'object') {
      const column = rel.rel_def.foreign_key_constraint_on;
      const fkc = findFKConstraint(curTable, column);
      if (fkc) {
        rtable = fkc.ref_table;
      }
    }

    // for array relationship
    if (rel.rel_type === 'array') {
      rtable = rel.rel_def.foreign_key_constraint_on.table;
      if (rtable.schema) {
        rtable = rtable.name;
      }
    }
  }

  return schemas.find(x => x.table_name === rtable);
};

const findAllFromRel = (schemas, curTable, rel) => {
  let rtable = null;
  let lcol;
  let rcol;

  const foreignKeyConstraintOn = rel.rel_def.foreign_key_constraint_on;

  // for view
  if (rel.rel_def.manual_configuration !== undefined) {
    rtable = rel.rel_def.manual_configuration.remote_table;

    if (rtable.schema) {
      rtable = rtable.name;
    }
    const columnMapping = rel.rel_def.manual_configuration.column_mapping;
    lcol = Object.keys(columnMapping)[0];
    rcol = columnMapping[lcol];
  }

  // for table
  if (foreignKeyConstraintOn !== undefined) {
    // for object relationship
    if (rel.rel_type === 'object') {
      lcol = foreignKeyConstraintOn;

      const fkc = findFKConstraint(curTable, lcol);
      if (fkc) {
        rtable = fkc.ref_table;
        rcol = fkc.column_mapping[lcol];
      }
    }

    // for array relationship
    if (rel.rel_type === 'array') {
      rtable = foreignKeyConstraintOn.table;
      rcol = foreignKeyConstraintOn.column;
      if (rtable.schema) {
        // if schema exists, its not public schema
        rtable = rtable.name;
      }

      const rtableSchema = schemas.find(x => x.table_name === rtable);
      const rfkc = findFKConstraint(rtableSchema, rcol);
      lcol = rfkc.column_mapping[rcol];
    }
  }

  return { lcol, rtable, rcol };
};

const getIngForm = string => {
  return (
    (string[string.length - 1] === 'e'
      ? string.slice(0, string.length - 1)
      : string) + 'ing'
  );
};

const getEdForm = string => {
  return (
    (string[string.length - 1] === 'e'
      ? string.slice(0, string.length - 1)
      : string) + 'ed'
  );
};

const escapeRegExp = string => {
  return string.replace(/([.*+?^${}()|[\]\\])/g, '\\$1');
};

const getTableColumns = (tableSchema) => {
  if (tableSchema) {
    return tableSchema.columns.map(colObj => colObj.column_name);
  }
  return [];
};

export {
  ordinalColSort,
  findTableFromRel,
  findAllFromRel,
  getEdForm,
  getIngForm,
  escapeRegExp,
  getTableColumns
};
