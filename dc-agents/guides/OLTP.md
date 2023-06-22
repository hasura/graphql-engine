# Hasura + SQL (OLTP)

What do people like doing with SQL DBs?

- Normalize their data so that its easy to ensure consistency
- Read short amounts of related data quickly by specifying filter/sort predicates
- Write data in transactions to ensure consistency

The Hasura + X story:

- Tables, views and functions that return relations become models
  - Models
- A transaction becomes a command that takes an input model, whose body is the invocation of the transaction, and the output relation is the output model
