const gqlPattern = /^[_A-Za-z][_0-9A-Za-z]*$/;

const gqlTableErrorNotif = [
  'Error creating table!',
  'Table name cannot contain special characters',
  {
    custom:
      'Table name cannot contain special characters. It can have letters, numbers and _ (cannot start with numbers)',
  },
  'Error renaming table!',
] as const;

const gqlColumnErrorNotif = [
  'Error adding column!',
  'Column name cannot contain special characters',
  {
    custom:
      'Column name cannot contain special characters. It can have letters, numbers and _ (cannot start with numbers)',
  },
  'Error renaming column!',
] as const;

const gqlViewErrorNotif = [
  'Error creating view!',
  'View name cannot contain special characters',
  {
    custom:
      'View name cannot contain special characters. It can have letters, numbers and _ (cannot start with numbers)',
  },
  'Error renaming view!',
] as const;

const gqlRelErrorNotif = [
  'Error adding relationship!',
  'Relationship name cannot contain special characters',
  {
    custom:
      'Relationship name cannot contain special characters. It can have letters, numbers and _ (cannot start with numbers)',
  },
  'Error renaming relationship!',
] as const;

const gqlSchemaErrorNotif = [
  'Error creating schema!',
  'Schema name cannot contain special characters',
  {
    custom:
      'Schema name cannot contain special characters. It can have letters, numbers and _ (cannot start with numbers)',
  },
] as const;

export default gqlPattern;
export {
  gqlTableErrorNotif,
  gqlViewErrorNotif,
  gqlColumnErrorNotif,
  gqlRelErrorNotif,
  gqlSchemaErrorNotif,
};
