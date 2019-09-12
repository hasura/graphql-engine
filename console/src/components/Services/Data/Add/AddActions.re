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


// Root fields aliasing

let _SET_ROOT_FIELD_ALIAS = "AddTable/SET_ROOT_FIELD_ALIAS";

type rfSelect = "select";


type _SET_ROOT_FIELD_ALIAS_ACTION = {
  .
  "type": string,
  "rootField": string,
  "alias": string
}
