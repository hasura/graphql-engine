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

function recurQueryDef(queryDef, fragments, definitionHash) {
  visit(queryDef, {
    FragmentSpread(node) {
      fragments.add(node.name.value);
      recurQueryDef(definitionHash[node.name.value], fragments, definitionHash);
    },
  });
}

const getQueryFragments = (queryDef, definitionHash = {}) => {
  const fragments = new Set();
  recurQueryDef(queryDef, fragments, definitionHash);
  return [...fragments];
};

const getQueryString = (queryDef, fragmentDefs, definitionHash = {}) => {
  let queryString = print(queryDef);

  const queryFragments = getQueryFragments(queryDef, definitionHash);

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
    throw new Error('Parsing operation failed');
  }

  const definitionHash = (parsedQueryString.definitions || []).reduce(
    (defObj, queryObj) => {
      defObj[queryObj.name.value] = queryObj;
      return defObj;
    },
    {}
  );

  const queryDefs = parsedQueryString.definitions.filter(
    def => def.kind === 'OperationDefinition'
  );

  const fragmentDefs = parsedQueryString.definitions.filter(
    def => def.kind === 'FragmentDefinition'
  );

  queryDefs.forEach(queryDef => {
    if (!queryDef.name) {
      throw new Error(`Operation without name found: ${print(queryDef)}`);
    }

    const query = {
      name: queryDef.name.value,
      query: getQueryString(queryDef, fragmentDefs, definitionHash),
    };

    queries.push(query);
  });

  const queryNames = queries.map(q => q.name);
  const duplicateNames = queryNames.filter(
    (q, i) => queryNames.indexOf(q) !== i
  );
  if (duplicateNames.length > 0) {
    throw new Error(
      `Operations with duplicate names found: ${duplicateNames.join(', ')}`
    );
  }

  return queries;
};
