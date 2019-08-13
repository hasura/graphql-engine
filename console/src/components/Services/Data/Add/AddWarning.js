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
};
