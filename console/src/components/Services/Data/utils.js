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

export const getSchemaQuery = options => {
  let whereQuery = '';
  const whereCondtions = [];
  if (
    (options.schemas && options.schemas.length !== 0) ||
    (options.tables && options.tables.length !== 0)
  ) {
    whereQuery = 'where';
  }
  if (options.schemas) {
    options.schemas.forEach(schemaName => {
      whereCondtions.push(`(ist.table_schema='${schemaName}')`);
    });
  }
  if (options.tables) {
    options.tables.forEach(tableInfo => {
      whereCondtions.push(
        `(ist.table_schema='${tableInfo.table_schema}' and ist.table_name='${
          tableInfo.table_name
        }')`
      );
    });
  }
  whereCondtions.forEach((whereInfo, index) => {
    whereQuery = whereQuery + ` ${whereInfo}`;
    if (index + 1 !== whereCondtions.length) {
      whereQuery = whereQuery + ' or';
    }
  });
  const runSql = `select 
  COALESCE(
    json_agg(
      row_to_json(info)
    ), 
    '[]' :: JSON
  ) AS tables 
FROM 
  (
    select 
      ist.table_schema, 
      ist.table_name, 
      obj_description(
        (
          ist.table_schema || '.' || ist.table_name
        ):: regclass, 
        'pg_class'
      ) as comment, 
      row_to_json(ist.*) as detail, 
      to_jsonb(
        array_remove(
          array_agg(
            DISTINCT row_to_json(isc) :: JSONB || jsonb_build_object(
              'comment', 
              (
                SELECT 
                  pg_catalog.col_description(
                    c.oid, isc.ordinal_position :: int
                  ) 
                FROM 
                  pg_catalog.pg_class c 
                WHERE 
                  c.oid = (
                    SELECT 
                      (
                        (
                          ist.table_schema || '.' || ist.table_name
                        ):: text
                      ):: regclass :: oid
                  ) 
                  AND c.relname = isc.table_name
              )
            )
          ), 
          NULL
        )
      ) AS columns, 
      to_jsonb(
        array_remove(
          array_agg(
            DISTINCT hdb_fkc.def :: JSONB
          ), 
          NULL
        )
      ) AS foreign_key_constraints, 
      to_jsonb(
        array_remove(
          array_agg(
            DISTINCT row_to_json(hdb_ofkc) :: JSONB || jsonb_build_object(
              'is_table_tracked', ofk_ref_table.table_name IS NOT NULL
            )
          ), 
          NULL
        )
      ) AS opp_foreign_key_constraints, 
      to_jsonb(
        array_remove(
          array_agg(
            DISTINCT row_to_json(hdb_uc) :: JSONB
          ), 
          NULL
        )
      ) AS unique_constraints, 
      row_to_json(hdb_pk.*) :: JSONB AS primary_key, 
      hdb_table.table_name IS NOT NULL AS is_table_tracked, 
      to_jsonb(
        array_remove(
          array_agg(
            DISTINCT row_to_json(hdb_rel) :: JSONB
          ), 
          NULL
        )
      ) AS relationships, 
      to_jsonb(
        array_remove(
          array_agg(
            DISTINCT row_to_json(hdb_perm) :: JSONB
          ), 
          NULL
        )
      ) AS permissions 
    from 
      information_schema.tables AS ist 
      LEFT OUTER JOIN information_schema.columns AS isc ON isc.table_schema = ist.table_schema 
      and isc.table_name = ist.table_name 
      LEFT OUTER JOIN (
        select
          row_to_json(hdb_fkc.*):: JSONB || jsonb_build_object(
            'is_ref_table_tracked', fk_ref_table.table_name IS NOT NULL
          ) || jsonb_build_object(
            'ref_table_columns', 
            array_agg(
              row_to_json(fkc_cols)
            )
          ) AS def 
        from 
          hdb_catalog.hdb_foreign_key_constraint AS hdb_fkc 
          LEFT OUTER JOIN hdb_catalog.hdb_table AS fk_ref_table ON fk_ref_table.table_schema = hdb_fkc.ref_table_table_schema 
          and fk_ref_table.table_name = hdb_fkc.ref_table 
          LEFT OUTER JOIN information_schema.columns AS fkc_cols ON fkc_cols.table_schema = hdb_fkc.ref_table_table_schema 
          and fkc_cols.table_name = hdb_fkc.ref_table 
        GROUP BY 
          hdb_fkc.table_schema, 
          hdb_fkc.table_name, 
          row_to_json(hdb_fkc.*):: JSONB, 
          fk_ref_table.table_name, 
          fk_ref_table.table_schema
      ) AS hdb_fkc ON hdb_fkc.def#>>'{table_schema}' = ist.table_schema 
      and hdb_fkc.def#>>'{table_name}' = ist.table_name 
      LEFT OUTER JOIN hdb_catalog.hdb_foreign_key_constraint AS hdb_ofkc ON hdb_ofkc.ref_table_table_schema = ist.table_schema 
      and hdb_ofkc.ref_table = ist.table_name 
      LEFT OUTER JOIN hdb_catalog.hdb_table AS ofk_ref_table ON ofk_ref_table.table_schema = hdb_ofkc.table_schema 
      and ofk_ref_table.table_name = hdb_ofkc.table_name 
      LEFT OUTER JOIN hdb_catalog.hdb_primary_key AS hdb_pk ON hdb_pk.table_schema = ist.table_schema 
      and hdb_pk.table_name = ist.table_name 
      LEFT OUTER JOIN hdb_catalog.hdb_unique_constraint AS hdb_uc ON hdb_uc.table_schema = ist.table_schema 
      and hdb_uc.table_name = ist.table_name 
      LEFT OUTER JOIN hdb_catalog.hdb_table AS hdb_table ON hdb_table.table_schema = ist.table_schema 
      and hdb_table.table_name = ist.table_name 
      LEFT OUTER JOIN hdb_catalog.hdb_relationship AS hdb_rel ON hdb_rel.table_schema = ist.table_schema 
      and hdb_rel.table_name = ist.table_name 
      LEFT OUTER JOIN hdb_catalog.hdb_permission_agg AS hdb_perm ON hdb_perm.table_schema = ist.table_schema 
      and hdb_perm.table_name = ist.table_name 
    ${whereQuery} 
    GROUP BY 
      ist.table_schema, 
      ist.table_name, 
      ist.*, 
      row_to_json(hdb_pk.*):: JSONB, 
      hdb_table.table_name
  ) AS info
`;
  return {
    type: 'run_sql',
    args: {
      sql: runSql,
    },
  };
};
