export const INTEGER = 'integer';
export const SERIAL = 'serial';
export const BIGINT = 'bigint';
export const BIGSERIAL = 'bigserial';
export const UUID = 'uuid';
export const JSONDTYPE = 'json';
export const JSONB = 'jsonb';
export const TIMESTAMP = 'timestamp with time zone';
export const TIME = 'time with time zone';
export const NUMERIC = 'numeric';
export const DATE = 'date';
export const TIMETZ = 'timetz';
export const BOOLEAN = 'boolean';
export const TEXT = 'text';

export const getPlaceholder = type => {
  switch (type) {
    case INTEGER:
      return 'integer';
    case BIGINT:
      return 'BIG integer';
    case NUMERIC:
      return 'float';
    case TIMESTAMP:
      return new Date().toISOString();
    case DATE:
      return new Date().toISOString().slice(0, 10);
    case TIMETZ:
      const time = new Date().toISOString().slice(11, 19);
      return `${time}Z or ${time}+05:30`;
    case UUID:
      return 'UUID';
    case JSON:
      return '{"name": "foo"} or [12, "bar"]';
    case JSONB:
      return '{"name": "foo"} or [12, "bar"]';
    case BOOLEAN:
      return '';
    default:
      return 'text';
  }
};

export const tabNameMap = {
  view: 'Browse Rows',
  insert: 'Insert Row',
  modify: 'Modify',
  relationships: 'Relationships',
  permissions: 'Permissions',
};

export const ordinalColSort = (a, b) => {
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
      Object.keys(fk.column_mapping).length === column.length &&
      Object.keys(fk.column_mapping).join(',') === column.join(',')
  );
};

export const findTableFromRel = (schemas, curTable, rel) => {
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
      const column = [rel.rel_def.foreign_key_constraint_on];
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

export const findAllFromRel = (schemas, curTable, rel) => {
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
    lcol = Object.keys(columnMapping);
    rcol = lcol.map(column => columnMapping[column]);
  }

  // for table
  if (foreignKeyConstraintOn !== undefined) {
    // for object relationship
    if (rel.rel_type === 'object') {
      lcol = [foreignKeyConstraintOn];

      const fkc = findFKConstraint(curTable, lcol);
      if (fkc) {
        rtable = fkc.ref_table;
        rcol = [fkc.column_mapping[lcol]];
      }
    }

    // for array relationship
    if (rel.rel_type === 'array') {
      rtable = foreignKeyConstraintOn.table;
      rcol = [foreignKeyConstraintOn.column];
      if (rtable.schema) {
        // if schema exists, its not public schema
        rtable = rtable.name;
      }

      const rtableSchema = schemas.find(x => x.table_name === rtable);
      const rfkc = findFKConstraint(rtableSchema, rcol);
      lcol = [rfkc.column_mapping[rcol]];
    }
  }
  return { lcol, rtable, rcol };
};

export const getIngForm = string => {
  return (
    (string[string.length - 1] === 'e'
      ? string.slice(0, string.length - 1)
      : string) + 'ing'
  );
};

export const getEdForm = string => {
  return (
    (string[string.length - 1] === 'e'
      ? string.slice(0, string.length - 1)
      : string) + 'ed'
  );
};

export const escapeRegExp = string => {
  return string.replace(/([.*+?^${}()|[\]\\])/g, '\\$1');
};

export const getTableName = t => {
  const typ = typeof t;
  if (typ === 'string') {
    return t;
  } else if (typ === 'object') {
    return 'name' in t ? t.name : '';
  }
  return '';
};

export const commonDataTypes = [
  {
    name: 'Integer',
    value: 'integer',
    description: 'signed four-byte integer',
    hasuraDatatype: 'integer',
  },
  {
    name: 'Integer (auto-increment)',
    value: 'serial',
    description: 'autoincrementing four-byte integer',
    hasuraDatatype: null,
  },
  {
    name: 'Text',
    value: 'text',
    description: 'variable-length character string',
    hasuraDatatype: 'text',
  },
  {
    name: 'Boolean',
    value: 'boolean',
    description: 'logical Boolean (true/false)',
    hasuraDatatype: 'boolean',
  },
  {
    name: 'Numeric',
    value: 'numeric',
    description: 'exact numeric of selected precision',
    hasuraDatatype: 'numeric',
  },
  {
    name: 'Timestamp',
    value: 'timestamptz',
    description: 'date and time, including time zone',
    hasuraDatatype: 'timestamp with time zone',
  },
  {
    name: 'Time',
    value: 'timetz',
    description: 'time of day (no time zone)',
    hasuraDatatype: 'time with time zone',
  },
  {
    name: 'Date',
    value: 'date',
    description: 'calendar date (year, month, day)',
    hasuraDatatype: 'date',
  },
  {
    name: 'UUID',
    value: 'uuid',
    description: 'universal unique identifier',
    hasuraDatatype: 'uuid',
  },
  {
    name: 'JSONB',
    value: 'jsonb',
    description: 'binary format JSON data',
    hasuraDatatype: 'jsonb',
  },
  {
    name: 'Big Integer',
    value: 'bigint',
    description: 'signed eight-byte integer',
    hasuraDatatype: 'bigint',
  },
  {
    name: 'Big Integer (auto-increment)',
    value: 'bigserial',
    description: 'autoincrementing eight-byte integer',
    hasuraDatatype: null,
  },
];

export const fetchColumnTypesQuery = `
SELECT 
  string_agg(t.typname, ',') as "Type Name",
  string_agg(pg_catalog.format_type(t.oid, NULL), ',') as "Display Name",
  string_agg(pg_catalog.obj_description(t.oid, 'pg_type'), ':') as "Descriptions",
  t.typcategory
FROM pg_catalog.pg_type t
     LEFT JOIN pg_catalog.pg_namespace n ON n.oid = t.typnamespace
WHERE (t.typrelid = 0 OR (SELECT c.relkind = 'c' FROM pg_catalog.pg_class c WHERE c.oid = t.typrelid))                                              
  AND NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type el WHERE el.oid = t.typelem AND el.typarray = t.oid)                                              
  AND pg_catalog.pg_type_is_visible(t.oid)
  AND t.typname != 'unknown'
  AND t.typcategory != 'P'
GROUP BY t.typcategory;`;
