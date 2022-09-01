export const column_mapping_input = {
  id: 'id',
  name: 'name',
};

export const column_name_data = [
  ['database', 'table_schema', 'table_name', 'column_name', 'data_type'],
  ['ddf0v5f7prohtg', 'public', 'testt', 'id', 'integer'],
  ['ddf0v5f7prohtg', 'public', 'testt', 'name', 'text'],
  ['ddf0v5f7prohtg', 'public', 'testt', 'firstname', 'text'],
  ['ddf0v5f7prohtg', 'public', 'testt', 'lastname', 'text'],
  ['ddf0v5f7prohtg', 'public', 'testt', 'weight', 'numeric'],
];

export const parse_rel_data = {
  definition: {
    to_source: {
      relationship_type: 'object',
      source: 'remote_db',
      table: { schema: 'public', name: 'testt' },
      field_mapping: { name: 'name', id: 'weight' },
    },
  },
  name: 'aaaaaaa',
};
