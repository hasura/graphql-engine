import { parse, print, visit, DefinitionNode } from 'graphql';

export type NewDefinitionNode = DefinitionNode & {
  name?: {
    value: string;
  };
};

export const readFileAsync = async (file: File | null): Promise<string> => {
  return new Promise((resolve, reject) => {
    const reader = new FileReader();
    reader.onload = (event) => {
      const content = event.target!.result as string;
      resolve(content);
    };

    reader.onerror = (event) => {
      reject(
        Error(`File could not be read! Code ${event.target!.error!.code}`),
      );
    };

    if (file) reader.readAsText(file);
  });
};

const recurQueryDef = (
  queryDef: NewDefinitionNode,
  fragments: Set<string>,
  definitionHash: Record<string, any>,
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
  definitionHash: Record<string, any> = {},
) => {
  const fragments = new Set<string>();
  recurQueryDef(queryDef, fragments, definitionHash);
  return [...Array.from(fragments)];
};

const getQueryString = (
  queryDef: NewDefinitionNode,
  fragmentDefs: NewDefinitionNode[],
  definitionHash: Record<string, any> = {},
) => {
  let queryString = print(queryDef);

  const queryFragments = getQueryFragments(queryDef, definitionHash);

  queryFragments.forEach((qf) => {
    const fragmentDef = fragmentDefs.find((fd) => {
      if (fd.name) return fd.name.value === qf;
      return undefined;
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
    {},
  );

  const queryDefs = definitions.filter(
    (def) => def.kind === 'OperationDefinition',
  );

  const fragmentDefs = definitions.filter(
    (def) => def.kind === 'FragmentDefinition',
  );

  queryDefs.forEach((queryDef) => {
    const queryName = queryDef.name ? queryDef.name.value : `unnamed`;

    const query = {
      name: queryName,
      query: getQueryString(queryDef, fragmentDefs, definitionHash),
    };

    queries.push(query);
  });

  return queries;
};
