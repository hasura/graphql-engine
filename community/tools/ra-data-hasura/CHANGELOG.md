## 0.0.6 (June 14, 2019)

- Bug Fix: Fix sort order, fix primary key when response not an array and add filters to GET_* operations.

## 0.0.5 (May 16, 2019)

- Feature: Support specifying primary keys other than id for tables using a config object 
Example:  `const config = { 'primaryKey': {'author': 'name'} }`

## 0.0.4 (April 25, 2019)

- Feature: Support multiple schemas using "." separator. 
Example:  `<Resource name="schema.table" />`

## 0.0.3 (January 24, 2019)

- Bug Fix: Fix count query to support UUID

## 0.0.2 (January 20, 2019)

- Bug Fix: GET_MANY_REFERENCE definition

## 0.0.1 (January 19, 2019)

- Add support for hasura data provider
