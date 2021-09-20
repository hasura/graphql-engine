import { parse, print, visit, DefinitionNode } from 'graphql';
import { AllowedQueriesCollection } from '../../../../metadata/reducer';
import { allowedQueriesCollection } from '../../../../metadata/utils';

export type NewDefinitionNode = DefinitionNode & {
  name?: {
    value: string;
  };
};

export const readFile = (
  file: File | null,
  callback: (content: string) => void
) => {
  const reader = new FileReader();
  reader.onload = event => {
    const content = event.target!.result as string;
    callback(content);
  };

  reader.onerror = event => {
    console.error(`File could not be read! Code ${event.target!.error!.code}`);
  };

  if (file) reader.readAsText(file);
};

const recurQueryDef = (
  queryDef: NewDefinitionNode,
  fragments: Set<string>,
  definitionHash: Record<string, any>
) => {
  visit(queryDef, {
    FragmentSpread(node) {
      fragments.add(node.name.value);
      recurQueryDef(definitionHash[node.name.value], fragments, definitionHash);
    },
  });
};

const getQueryFragments = (
  queryDef: NewDefinitionNode,
  definitionHash: Record<string, any> = {}
) => {
  const fragments = new Set<string>();
  recurQueryDef(queryDef, fragments, definitionHash);
  return [...Array.from(fragments)];
};

const getQueryString = (
  queryDef: NewDefinitionNode,
  fragmentDefs: NewDefinitionNode[],
  definitionHash: Record<string, any> = {}
) => {
  let queryString = print(queryDef);

  const queryFragments = getQueryFragments(queryDef, definitionHash);

  queryFragments.forEach(qf => {
    // eslint-disable-next-line array-callback-return
    const fragmentDef = fragmentDefs.find(fd => {
      if (fd.name) return fd.name.value === qf;
    });

    if (fragmentDef) {
      queryString += `\n\n${print(fragmentDef)}`;
    }
  });

  return queryString;
};

// parses the query string and returns an array of queries
export const parseQueryString = (queryString: string) => {
  const queries: { name: string; query: string }[] = [];

  let parsedQueryString;

  try {
    parsedQueryString = parse(queryString);
  } catch (ex) {
    throw new Error('Parsing operation failed');
  }

  const definitions: NewDefinitionNode[] = [...parsedQueryString.definitions];

  const definitionHash = (definitions || []).reduce(
    (defObj: Record<string, NewDefinitionNode>, queryObj) => {
      if (queryObj.name) defObj[queryObj.name.value] = queryObj;
      return defObj;
    },
    {}
  );

  const queryDefs = definitions.filter(
    def => def.kind === 'OperationDefinition'
  );

  const fragmentDefs = definitions.filter(
    def => def.kind === 'FragmentDefinition'
  );

  queryDefs.forEach(queryDef => {
    const queryName = queryDef.name ? queryDef.name.value : `unnamed`;

    const query = {
      name: queryName,
      query: getQueryString(queryDef, fragmentDefs, definitionHash),
    };

    queries.push(query);
  });

  return queries;
};

export const getQueriesInCollection = (
  collectionName: string,
  allowedQueries: AllowedQueriesCollection[]
) => {
  const queries: AllowedQueriesCollection[] = [];
  allowedQueries.forEach(query => {
    if (query.collection === collectionName) {
      queries.push(query);
    }
  });
  return queries;
};

// check if the uploaded queries have same names within the file, or among the already present queries
export const renameDuplicates = (
  fileQueries: { name: string; query: string }[],
  allQueries: AllowedQueriesCollection[]
) => {
  // we only allow addition to allowedQueriesCollection from console atm
  const allowListQueries = getQueriesInCollection(
    allowedQueriesCollection,
    allQueries
  );

  const queryNames = new Set();
  allowListQueries.forEach(query => queryNames.add(query.name));

  const updatedQueries = fileQueries.map(query => {
    let queryName = query.name;
    if (queryNames.has(queryName)) {
      let num = 1;
      while (queryNames.has(queryName)) {
        queryName = `${query.name}_${num++}`;
      }
    }
    queryNames.add(queryName);
    return { name: queryName, query: query.query };
  });

  return updatedQueries;
};

export const checkLastQuery = (
  collectionName: string,
  queries: AllowedQueriesCollection[]
) => {
  return getQueriesInCollection(collectionName, queries).length === 1;
};

// Missing feature in typescript https://stackoverflow.com/questions/33464504/using-spread-syntax-and-new-set-with-typescript/33464709
export const getCollectionNames = (queries: AllowedQueriesCollection[]) => {
  return Array.from(new Set(queries.map(query => query.collection)));
};
