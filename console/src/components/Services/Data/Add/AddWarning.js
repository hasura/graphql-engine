const tableColumnNoDupsNotif = [
  'Error adding column',
  'Column name is duplicated',
  {
    custom: 'Column name is duplicated',
  },
];

const tableMinPrimaryKeyNotif = [
  'Error adding table',
  'A primary key is required',
  {
    custom: 'A primary key is required',
  },
];

const tableNameNullNotif = [
  'Error creating table!',
  'Table name cannot be empty',
  {
    custom: 'Table name cannot be empty. Please add a name',
  },
];

const tableNameMaxLengthNotif = [
  'Error creating table!',
  'Table name should be less than 64 characters',
  {
    custom: 'Table name should be less than 64 characters',
  },
];

const tableEnufColumnsNotif = [
  'Error creating table!',
  'Table must have at least one column',
  {
    custom: 'Table must have at least one column.',
  },
];

const tableColumnDefaultsNotif = [
  'Error creating table!',
  'Default value is invalid',
  {
    custom: 'Default value is invalid',
  },
];

const tableColumnMaxLengthNotif = [
  'Error creating table!',
  'Column names should be less than 64 characters',
  {
    custom: 'Column names should be less than 64 characters',
  },
];

const tableColumnTypesNotif = [
  'Error creating table!',
  'Column type is invalid',
  {
    custom: 'Column type is invalid',
  },
];

export {
  tableNameNullNotif,
  tableEnufColumnsNotif,
  tableColumnNoDupsNotif,
  tableMinPrimaryKeyNotif,
  tableColumnDefaultsNotif,
  tableColumnTypesNotif,
  tableColumnMaxLengthNotif,
  tableNameMaxLengthNotif,
};
