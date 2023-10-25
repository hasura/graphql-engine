export const databaseNamingScheme = `
Recommend using snake_case naming convertion for table and columns. eg:
product_skus
`;

export const columnsDescription = `
Any data type can be used as an array type. Enable the "Array" checkbox to convert the data type to array.
`;

export const primaryKeyDescription = `
A primary-key is a column (or set of columns) that uniquely identifies a row
of a table
`;

export const foreignKeyDescription = `
A foreign-key is a column (or set of columns) in one table that uniquely
identifies a row of another table (can be the same table). They are used to
establish a link between the data in the two tables.
`;

export const uniqueKeyDescription = `
A unique key is a column (or set of columns) in a table that has unique
values across the table.
`;

export const checkConstraintsDescription = `
A check constraint allows you to specify if the value in a certain column
must satisfy a specific condition.
`;

export const fkViolationOnUpdate = `
Action when the reference key is updated
`;

export const fkViolationOnDelete = `
Action when the reference key is deleted
`;

export const checkConstraintExpression = `
Boolean expression that must be satisfied for all rows in the table. e.g. min_price >= 0 AND max_price >= min_price
`;

export const indexFieldsDescription = `
Indexes are used to increase query performance based on columns that are queried frequently
`;
