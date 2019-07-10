// check 2xx success status codes
export const verifySuccessStatus = status => {
  return /^2[0-9][0-9]$/.test(status.toString());
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
      Object.keys(fk.column_mapping).length === 1 &&
      Object.keys(fk.column_mapping)[0] === column
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

export const getTableColumns = tableSchema => {
  if (tableSchema) {
    return tableSchema.columns.map(colObj => ({
      name: colObj.column_name,
      type: colObj.udt_name,
    }));
  }
  return [];
};

export const convertDateTimeToLocale = dateTime => {
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

export const getEventTriggersQuery = triggerNames => {
  let whereQuery = '';
  const triggerLength = triggerNames.length;
  if (triggerLength !== 0) {
    // append to whereQuery
    whereQuery = 'where';
    triggerNames.forEach((triggerName, index) => {
      whereQuery = whereQuery + ` hdb_et.name='${triggerName}'`;
      if (index + 1 !== triggerLength) {
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

export const parseRowData = (row, dataType) => {
  switch (dataType) {
    case 'request':
      switch (row.request.version) {
        case '2':
          const data = row.request.payload;
          return {
            data: data,
            headers: row.request.headers,
          };
        default:
          return {
            data: row.request,
          };
      }
    case 'response':
      let data;
      switch (row.response.version) {
        case '2':
          try {
            // Handle graphql-engine server error message
            if (row.response.data.message) {
              data = row.response.data;
            } else {
              data = JSON.parse(row.response.data.body);
            }
          } catch (e) {
            console.error(e);
            data = row.response.data.body;
          }
          return {
            data: data,
            headers: row.response.data.headers,
            status_code: row.response.data.status,
          };
        default:
          try {
            data = JSON.parse(row.response);
          } catch (e) {
            console.error(e);
            data = row.response;
          }
          return {
            data: data,
            status_code: row.status,
          };
      }
    default:
      return false;
  }
};
