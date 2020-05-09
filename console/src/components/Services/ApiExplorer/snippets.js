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
  generate: arg => `import React from "react"
import { graphql } from "graphql"
const ComponentName = ({ data }) => <pre>{JSON.stringify(data, null, 4)}</pre>
export const query = graphql\`
${getQuery(arg, 2)}
\`
export default ComponentName
`,
};

export default [snippets[0], typescriptSnippet];
