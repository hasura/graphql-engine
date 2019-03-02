const gqlPattern = /^[_A-Za-z][_0-9A-Za-z]*$/;

const gqlTableErrorNotif = [
  'Error creating table!',
  'Table name cannot contain special characters',
  '',
  {
    custom:
      'Table name cannot contain special characters. It can have alphabets, numbers (cannot start with numbers) and _ (can start with _)',
  },
  'Error renaming table!',
];

const gqlColumnErrorNotif = [
  'Error adding column!',
  'Column name cannot contain special characters',
  '',
  {
    custom:
      'Column name cannot contain special characters. It can have alphabets, numbers (cannot start with numbers) and _ (can start with _)',
  },
  'Error renaming column!',
];

const gqlViewErrorNotif = [
  'Error creating view!',
  'View name cannot contain special characters',
  '',
  {
    custom:
      'View name cannot contain special characters. It can have alphabets, numbers (cannot start with numbers) and _ (can start with _)',
  },
  'Error renaming view!',
];

const gqlRelErrorNotif = [
  'Error adding relationship!',
  'Relationship name cannot contain special characters',
  '',
  {
    custom:
      'Relationship name cannot contain special characters. It can have alphabets, numbers (cannot start with numbers) and _ (can start with _)',
  },
  'Error renaming relationship!',
];

export default gqlPattern;
export {
  gqlTableErrorNotif,
  gqlViewErrorNotif,
  gqlColumnErrorNotif,
  gqlRelErrorNotif,
};
