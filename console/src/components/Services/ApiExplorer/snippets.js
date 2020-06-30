import snippets from 'graphiql-code-exporter/lib/snippets';

const getQuery = (arg, spaceCount) => {
  const { operationDataList } = arg;
  const { query } = operationDataList[0];
  const anonymousQuery = query.replace(/query\s.+{/gim, `{`);
  return (
    ` `.repeat(spaceCount) +
    anonymousQuery.replace(/\n/g, `\n` + ` `.repeat(spaceCount))
  );
};

const typescriptSnippet = {
  name: `Page query`,
  language: `Typescript`,
  codeMirrorMode: `jsx`,
  options: [],
  generate: arg => `
function fetchGraphQL(
  operationsDoc: string,
  operationName: string,
  variables: Record<string, any>
) {
  return fetch('undefined', {
    method: 'POST',
    body: JSON.stringify({
      query: operationsDoc,
      variables,
      operationName,
    }),
  }).then(result => result.json());
}

const operationsDoc = \`
# Consider giving this query a unique, descriptive
# name in your application as a best practice

${getQuery(arg, 2)}
\`;

function fetchUnnamedQuery1() {
  return fetchGraphQL(operationsDoc, <QUERY_NAME>, {});
}

fetchUnnamedQuery1()
  .then(({ data, errors }) => {
    if (errors) {
      console.error(errors);
    }
    console.log(data);
  })
  .catch(error => {
    console.error(error);
  });
`,
};

export default [snippets[0], typescriptSnippet];
