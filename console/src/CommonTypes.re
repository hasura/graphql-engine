type qualifiedTable = {
  .
  "name": string,
  "schema": string
};

type customRootFields = {
  .
  "select": string,
  "select_by_pk": string,
  "select_aggregate": string,
  "insert": string,
  "update": string,
  "delete": string
};

type _argsSetCustomRootFields = {
  .
  "table": string,
  "custom_root_fields": customRootFields,
  "custom_column_names": Js.Dict.t(string)
};

type _querySetTableCustomFields = {
  .
  "type": string,
  "version": int,
  "args": _argsSetCustomRootFields
};