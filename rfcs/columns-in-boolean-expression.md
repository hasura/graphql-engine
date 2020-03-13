## Allow customizing a table's boolean expression.

Currently, a table's boolean expression for a role is generated with the
columns that are allowed to be read as defined in the select permission.
Sometimes you may want to restrict this further, for example you may want to
only allow columns on which indexes are defined.

#### Proposed solution

Introduce an optional `boolean_expression_columns` field in select permission
which specifies the columns that are allowed in a boolean expression. To
preserve backwards compatibility, when this field is absent in a select
permission, `columns` is used for generating the boolean expression.

Note: If a table's primary key column set is not a subset of
of `boolean_expression_columns`, we shouldn't generate `<table>_by_pk` field.
