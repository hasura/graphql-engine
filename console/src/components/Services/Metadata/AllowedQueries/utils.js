import { parse, print } from 'graphql';

export const readFile = (file, callback) => {
  const reader = new FileReader();
  reader.onload = event => {
    const content = event.target.result;
    callback(content);
  };

  reader.onerror = event => {
    console.error('File could not be read! Code ' + event.target.error.code);
  };

  reader.readAsText(file);
};

export const parseQueryString = queryString => {
  const queries = [];

  let parsedQuery;

  try {
    parsedQuery = parse(queryString);
  } catch (ex) {
    throw new Error('Parsing query failed');
  }

  const queryDefs = parsedQuery.definitions.filter(
    qd => qd.kind === 'OperationDefinition'
  );

  queryDefs.forEach(queryDef => {
    if (!queryDef.name) {
      throw new Error(`Query without name found: ${print(queryDef)}`);
    }

    const query = {
      name: queryDef.name.value,
      query: print(queryDef),
    };

    queries.push(query);
  });

  const queryNames = queries.map(q => q.name);
  const duplicateNames = queryNames.filter(
    (q, i) => queryNames.indexOf(q) !== i
  );
  if (duplicateNames.length > 0) {
    throw new Error(
      `Queries with duplicate names found: ${duplicateNames.join(', ')}`
    );
  }

  return queries;
};
