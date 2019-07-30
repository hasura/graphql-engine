const tableColumnNoDups = [
  'Error adding column',
  'Column name is duplicated',
  {
    custom: 'Column name is duplicated',
  },
];

const tableMinPrimaryKey = [
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

const tableEnufColumns = [
  'Error creating table!',
  'Table must have at least one column',
  {
    custom: 'Table must have at least one column.',
  },
];

const tableColumnDefaults = [
  'Error creating table!',
  'Default value is invalid',
  {
    custom: 'Default value is invalid',
  },
];

const tableColumnTypes = [
  'Error creating table!',
  'Column type is invalid',
  {
    custom: 'Column type is invalid',
  },
];

export {
  tableNameNullNotif,
  tableEnufColumns,
  tableColumnNoDups,
  tableMinPrimaryKey,
  tableColumnDefaults,
  tableColumnTypes,
};
