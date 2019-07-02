const gqlPattern = /^[_A-Za-z][_0-9A-Za-z]*$/;

const gqlTableErrorNotif = [
  'Error creating table!',
  'Table name cannot contain special characters',
  '',
  {
    custom:
      'Table name cannot contain special characters. It can have alphabets, numbers and _ (cannot start with numbers)',
  },
  'Error renaming table!',
];

const gqlColumnErrorNotif = [
  'Error adding column!',
  'Column name cannot contain special characters',
  '',
  {
    custom:
      'Column name cannot contain special characters. It can have alphabets, numbers and _ (cannot start with numbers)',
  },
  'Error renaming column!',
];

const gqlColumnNoDups = [
  'Error adding column',
  'Column name is duplicated',
  '',
  {
    custom: 'Column name is duplicated',
  },
];

const gqlMinPrimaryKey = [
  'Error adding table',
  'A primary key is required',
  '',
  {
    custom: 'A primary key is required',
  },
];

const gqlViewErrorNotif = [
  'Error creating view!',
  'View name cannot contain special characters',
  '',
  {
    custom:
      'View name cannot contain special characters. It can have alphabets, numbers and _ (cannot start with numbers)',
  },
  'Error renaming view!',
];

const gqlRelErrorNotif = [
  'Error adding relationship!',
  'Relationship name cannot contain special characters',
  '',
  {
    custom:
      'Relationship name cannot contain special characters. It can have alphabets, numbers and _ (cannot start with numbers)',
  },
  'Error renaming relationship!',
];

const gqlSchemaErrorNotif = [
  'Error creating schema!',
  'Schema name cannot contain special characters',
  '',
  {
    custom:
      'Schema name cannot contain special characters. It can have alphabets, numbers and _ (cannot start with numbers)',
  },
];

const gqlTableNameNullNotif = [
  'Error creating table!',
  'Table name cannot be empty',
  '',
  {
    custom: 'Table name cannot be empty. Please add a name',
  },
];

const gqlTableEnufColumns = [
  'Error creating table!',
  'Table must have at least one column',
  '',
  {
    custom: 'Table must have at least one column.',
  },
];

const gqlColumnDefaults = [
  'Error creating table!',
  'Default value is invalid',
  '',
  {
    custom: 'Default value is invalid',
  },
];

const gqlColumnTypes = [
  'Error creating table!',
  'Column type is invalid',
  '',
  {
    custom: 'Column type is invalid',
  },
];

export default gqlPattern;
export {
  gqlTableErrorNotif,
  gqlViewErrorNotif,
  gqlColumnErrorNotif,
  gqlRelErrorNotif,
  gqlSchemaErrorNotif,
  gqlTableNameNullNotif,
  gqlTableEnufColumns,
  gqlColumnNoDups,
  gqlMinPrimaryKey,
  gqlColumnDefaults,
  gqlColumnTypes,
};
