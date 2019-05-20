import { parse, print, visit } from 'graphql';

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

const getQueryFragments = queryDef => {
  const fragments = [];

  visit(queryDef, {
    FragmentSpread(node) {
      fragments.push(node.name.value);
    },
  });

  return fragments;
};

const getQueryString = (queryDef, fragmentDefs) => {
  let queryString = print(queryDef);

  const queryFragments = getQueryFragments(queryDef);

  queryFragments.forEach(qf => {
    const fragmentDef = fragmentDefs.find(fd => fd.name.value === qf);

    if (fragmentDef) {
      queryString += '\n\n' + print(fragmentDef);
    }
  });

  return queryString;
};

export const parseQueryString = queryString => {
  const queries = [];

  let parsedQueryString;

  try {
    parsedQueryString = parse(queryString);
  } catch (ex) {
    throw new Error('Parsing query failed');
  }

  const queryDefs = parsedQueryString.definitions.filter(
    def => def.kind === 'OperationDefinition'
  );

  const fragmentDefs = parsedQueryString.definitions.filter(
    def => def.kind === 'FragmentDefinition'
  );

  queryDefs.forEach(queryDef => {
    if (!queryDef.name) {
      throw new Error(`Query without name found: ${print(queryDef)}`);
    }

    const query = {
      name: queryDef.name.value,
      query: getQueryString(queryDef, fragmentDefs),
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
