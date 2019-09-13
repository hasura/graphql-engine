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
