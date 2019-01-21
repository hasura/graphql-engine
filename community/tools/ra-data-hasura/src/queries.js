// define hasura json api queries
const bulkQuery = {
  type: 'bulk',
  args: []
};

const selectQuery = {
  type: 'select',
  args: {
    table: '',
    columns: ['*']
  }
};

const countQuery = {
  type: 'count',
  args: {
    table: '',
    where: { id: { '$gt': 0 }}
  }
};

const insertQuery = {
  type: 'insert',
  args: {
    table: '',
    objects: [],
    returning: []
  }
};

const updateQuery = {
  type: 'update',
  args: {
    table: '',
    $set: {},
    where: {},
    returning: []
  }
};

const deleteQuery = {
  type: 'delete',
  args: {
    table: '',
    where: {},
    returning: []
  }
};

export { bulkQuery, selectQuery, countQuery, insertQuery, updateQuery, deleteQuery };

