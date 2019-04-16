// define hasura json api queries
const bulkQuery = {
  type: 'bulk',
  args: []
};

const selectQuery = {
  type: 'select',
  args: {
    table: { 'schema': 'public', 'name': '' },
    columns: ['*']
  }
};

const countQuery = {
  type: 'count',
  args: {
    table: { 'schema': 'public', 'name': '' },
    where: { id: { '$ne': null }}
  }
};

const insertQuery = {
  type: 'insert',
  args: {
    table: { 'schema': 'public', 'name': '' },
    objects: [],
    returning: []
  }
};

const updateQuery = {
  type: 'update',
  args: {
    table: { 'schema': 'public', 'name': '' },
    $set: {},
    where: {},
    returning: []
  }
};

const deleteQuery = {
  type: 'delete',
  args: {
    table: { 'schema': 'public', 'name': '' },
    where: {},
    returning: []
  }
};

export { bulkQuery, selectQuery, countQuery, insertQuery, updateQuery, deleteQuery };
