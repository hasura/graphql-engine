// Column Aliasing

let _SET_COLUMN_ALIAS = "AddTable/SET_COL_ALIAS";
type _SET_COLUMN_ALIAS_ACTION = {
  .
  "type": string,
  "alias": string,
  "index": int
};
let setColumnAlias: (string, int) => _SET_COLUMN_ALIAS_ACTION
  = (alias, index) => {
    {
      "type": _SET_COLUMN_ALIAS,
      "alias": alias,
      "index": index
    }
  };

let _SET_ROOT_FIELD_ALIAS = "AddTable/SET_ROOT_FIELD_ALIAS";

let rootFieldTypes = [| "select", "select_by_pk", "select_aggregate", "insert", "update", "delete" |];

type _SET_ROOT_FIELD_ALIAS_ACTION = {
  .
  "type": string,
  "field": string,
  "alias": string
};

let setRootFieldAlias: (string, string) => _SET_ROOT_FIELD_ALIAS_ACTION
  = (field, alias) => {
    {
      "type": _SET_ROOT_FIELD_ALIAS,
      "field": field,
      "alias": alias
    }
  };

let setDefaultAliases = (tableName, dispatch) => {
  Array.iteri(
    (_, rootFieldType) => {
      if (Js.String.includes(rootFieldType, "select")) {
        dispatch(
          setRootFieldAlias(rootFieldType, Js.String.replace("select", tableName, rootFieldType))
        )
      } else {
        dispatch(
          setRootFieldAlias(rootFieldType, rootFieldType ++ "_" ++ tableName)
        )
      }
    },
    rootFieldTypes
  );
  ()
};
