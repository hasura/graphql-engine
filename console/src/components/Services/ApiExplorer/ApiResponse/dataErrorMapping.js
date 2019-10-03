const dataErrorMapping = {
  'postgres-error':
    'When inserting into table, all columns which are not nullable needs to be given a value.',
  'permission-denied':
    'Looks like current role doesnt have permissions to perform this operation on this table. Add permission to execute this query',
  'not-exists':
    'Looks like either the table or column name doesnt exist. Modify the query and try again',
  'already-tracked': 'The view/table has already been tracked.',
  'access-denied':
    'The action you are trying to perform is admin only. Login as an admin and try again',
  'not-supported': 'Table renames are not supported',
  'already-exists':
    'The column name already exists as a relationship name. Try with a different name.',
  'invalid-json': 'Request body is not a valid json',
  'invalid-headers':
    'Expected headers are missing or not in the right format in the request. Modify them before making the query.',
  'dependency-error':
    'There is a dependency issue. Look at the response for the dependent objects',
  'parse-failed': 'Parsing table failed',
  'already-initialised':
    'The state seems to be initialised already. You may need to migrate from the given catalog version',
  'constraint-error':
    'There may be no foreign key constraint / multiple foreign key constraints on the given column',
  'permission-error': 'Permission for the given role exists/does not exist',
  'unexpected-payload': 'Check the response for more details.',
  'invalid-params':
    'Parameter is missing. Check the response for missing parameter',
  unexpected: 'Internal Server Error. Check the response for more details',
  'not-found':
    'No such endpoint exists. Check the URL/Method for which the query is being made to.',
};

export default dataErrorMapping;
