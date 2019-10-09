type qualifiedTable = {
  .
  "name": string,
  "schema": string
};

type customRootFields = {
  .
  "select": option(string),
  "select_by_pk": option(string),
  "select_aggregate": option(string),
  "insert": option(string),
  "update": option(string),
  "delete": option(string),
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