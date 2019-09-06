import { getDataTypeInfo } from '../Common/utils';

const convertArrayToJson = (arr, keyIndex = 0) => {
  const converted = {};
  arr.forEach(a => {
    converted[a[keyIndex]] = a;
  });
  return converted;
};

const getValidAlterOptions = (alterTypeOptions, colName) => {
  const { typInfo: currentInfo, typValueMap: currentMap } = getDataTypeInfo(
    alterTypeOptions.slice(0, 3),
    colName,
    0
  );
  /*
   * alterTypeOptions can also only contain only three elements
   */
  let allInfo = [...currentInfo];
  let allOptionsMap = {
    ...currentMap,
  };

  if (alterTypeOptions.length > 3) {
    const {
      typInfo: validOptions,
      typValueMap: validOptionsMap,
    } = getDataTypeInfo(alterTypeOptions.slice(3, 6), colName, 0);

    allInfo = allInfo.concat(validOptions);
    // const allInfo = [...currentInfo, ...validOptions];
    allOptionsMap = {
      ...validOptionsMap,
      ...currentMap,
    };
  }

  return {
    alterOptions: allInfo,
    alterOptionsValueMap: allOptionsMap,
  };
};

const fetchColumnCastsQuery = `
SELECT ts.typname AS "Source Type",
       pg_catalog.format_type(castsource, NULL) AS "Source Info",
       coalesce(pg_catalog.obj_description(castsource, 'pg_type'), '') as "Source Descriptions",
       string_agg(tt.typname, ',') AS "Target Type",
       string_agg(pg_catalog.format_type(casttarget, NULL), ',') AS "Target Info",
       string_agg(coalesce(pg_catalog.obj_description(casttarget, 'pg_type'), ''), ':') as "Target Descriptions",
       string_agg(CASE WHEN castfunc = 0 THEN '(binary coercible)'
            ELSE p.proname
       END, ',') as "Function"
     FROM pg_catalog.pg_cast c LEFT JOIN pg_catalog.pg_proc p
     ON c.castfunc = p.oid
     LEFT JOIN pg_catalog.pg_type ts
     ON c.castsource = ts.oid
     LEFT JOIN pg_catalog.pg_namespace ns
     ON ns.oid = ts.typnamespace
     LEFT JOIN pg_catalog.pg_type tt
     ON c.casttarget = tt.oid
     LEFT JOIN pg_catalog.pg_namespace nt
     ON nt.oid = tt.typnamespace
WHERE ( (true  AND pg_catalog.pg_type_is_visible(ts.oid)
) OR (true  AND pg_catalog.pg_type_is_visible(tt.oid)
) ) AND (c.castcontext != 'e') AND ts.typname != tt.typname
GROUP BY ts.typname, castsource
ORDER BY 1, 2;

`;

const getCreatePkSql = ({
  schemaName,
  tableName,
  selectedPkColumns,
  constraintName,
}) => {
  return `alter table "${schemaName}"."${tableName}"
    add constraint "${constraintName}" 
    primary key ( ${selectedPkColumns.map(pkc => `"${pkc}"`).join(', ')} );`;
};

const getDropPkSql = ({ schemaName, tableName, constraintName }) => {
  return `alter table "${schemaName}"."${tableName}" drop constraint "${constraintName}";`;
};

export {
  convertArrayToJson,
  getValidAlterOptions,
  fetchColumnCastsQuery,
  getCreatePkSql,
  getDropPkSql,
};
