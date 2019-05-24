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

const getTableColumns = tableSchema => {
  if (tableSchema) {
    return tableSchema.columns.map(colObj => ({
      name: colObj.column_name,
      type: colObj.udt_name,
    }));
  }
  return [];
};

const convertDateTimeToLocale = dateTime => {
  const options = {
    hourCycle: 'h24',
    day: 'numeric',
    hour: '2-digit',
    minute: '2-digit',
    month: 'short',
    second: '2-digit',
    timeZoneName: 'short',
    weekday: 'short',
    year: 'numeric',
  };

  return new Date(dateTime + 'Z').toLocaleString('en-US', options);
};

const getEventTriggersQuery = (triggerNames) => {
  let whereQuery = '';
  const triggerLength = triggerNames.length;
  if (triggerLength !== 0) {
    // append to whereQuery
    whereQuery = 'where';
    triggerNames.forEach((triggerName, index) => {
      whereQuery = whereQuery + ` hdb_et.name='${triggerName}'`;
      if ((index + 1) !== triggerLength) {
        whereQuery = whereQuery + ' and';
      }
    });
  }
  const runSql = `select 
  COALESCE(
    json_agg(
      row_to_json(info)
    ), 
    '[]' :: JSON
  ) AS event_triggers 
FROM 
  (
    select 
      hdb_et.name, 
      hdb_et.type, 
      hdb_et.schema_name AS table_schema, 
      hdb_et.table_name, 
      hdb_et.configuration, 
      to_jsonb(
        array_remove(
          array_agg(
            DISTINCT row_to_json(isc) :: JSONB
          ), 
          NULL
        )
      ) AS columns, 
      row_to_json(hdb_pk.*) :: JSONB AS primary_key 
    from 
      hdb_catalog.event_triggers AS hdb_et 
      JOIN hdb_catalog.hdb_table AS hdb_table ON hdb_table.table_schema = hdb_et.schema_name 
      and hdb_table.table_name = hdb_et.table_name 
      JOIN information_schema.columns AS isc ON isc.table_schema = hdb_table.table_schema 
      and isc.table_name = hdb_table.table_name 
      LEFT OUTER JOIN hdb_catalog.hdb_primary_key AS hdb_pk ON hdb_pk.table_schema = hdb_table.table_schema 
      and hdb_pk.table_name = hdb_table.table_name
    ${whereQuery}
    GROUP BY 
      hdb_et.name, 
      row_to_json(hdb_pk.*):: JSONB 
    ORDER BY 
      hdb_et.name ASC NULLS LAST
  ) AS info
`;
  return {
    type: 'run_sql',
    args: {
      sql: runSql,
    },
  };
};

export {
  ordinalColSort,
  findTableFromRel,
  findAllFromRel,
  getEdForm,
  getIngForm,
  escapeRegExp,
  getTableColumns,
  convertDateTimeToLocale,
  getEventTriggersQuery,
};
