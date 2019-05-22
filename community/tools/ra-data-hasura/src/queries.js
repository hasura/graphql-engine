// define hasura json api queries
const bulkQuery = {
  type: 'bulk',
  args: []
};

const selectQuery = {
  type: 'select',
  args: {
    table: {'schema': '', 'name': ''},
    columns: ['*']
  }
};

const countQuery = {
  type: 'count',
  args: {
    table: {'schema': '', 'name': ''},
    where: {}
  }
};

const insertQuery = {
  type: 'insert',
  args: {
    table: {'schema': '', 'name': ''},
    objects: [],
    returning: []
  }
};

const updateQuery = {
  type: 'update',
  args: {
    table: {'schema': '', 'name': ''},
    $set: {},
    where: {},
    returning: []
  }
};

const deleteQuery = {
  type: 'delete',
  args: {
    table: {'schema': '', 'name': ''},
    $set: {},
    where: {},
    returning: []
  }
};

export { bulkQuery, selectQuery, countQuery, insertQuery, updateQuery, deleteQuery };

