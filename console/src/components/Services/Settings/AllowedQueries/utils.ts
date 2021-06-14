import { parse, print, visit, DefinitionNode } from 'graphql';
import { AllowedQueriesCollection } from '../../../../metadata/reducer';

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
