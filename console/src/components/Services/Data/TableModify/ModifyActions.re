open CommonUtils;

let _MODIFY_ROOT_FIELD_ALIASES = "ModifyTable/MODIFY_ROOT_FIELD_ALIASES";

type _MODIFY_ROOT_FIELD_ALIASES_ACTION = {
  .
  "type": string,
  "field": string,
  "alias": string
};

let modifyRootFields = (field, alias) => {
  {
    "type": _MODIFY_ROOT_FIELD_ALIASES,
    "field": field,
    "alias": alias
  }
};

let sanitiseRootFields = (rootFields) => {
  {
    "select": stringToOption(rootFields##select),
    "select_by_pk": stringToOption(rootFields##select_by_pk),
    "select_aggregate": stringToOption(rootFields##select_aggregate),
    "insert": stringToOption(rootFields##insert),
    "update": stringToOption(rootFields##update),
    "delete": stringToOption(rootFields##delete)
  }
}

let generateAliasingQuery = (
  newRootFields,
  newColumnNames,
  oldRootFields,
  oldColumnNames,
  tableName,
  schemaName
) => {

  let generateQuery = (cRootFields, cColumnNames) => {
    {
      "type": "set_table_custom_fields",
      "version": 2,
      "args": {
        "table": {
          "name": tableName,
          "schema": schemaName
        },
        "custom_root_fields": sanitiseRootFields(cRootFields),
        "custom_column_names": cColumnNames
      }
    }
  };

  let upQuery = generateQuery(newRootFields, newColumnNames);
  let downQuery = generateQuery(oldRootFields, oldColumnNames);

  (upQuery, downQuery);

};