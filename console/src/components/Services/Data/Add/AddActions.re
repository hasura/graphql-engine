let _SET_COLUMN_ALIAS= "AddTable/SET_COL_ALIAS";

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

