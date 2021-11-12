import React from 'react';
import GraphiQL from 'graphiql';
import BrowserOnly from '@docusaurus/BrowserOnly';
import cslx from 'clsx';
import { createGraphiQLFetcher } from '@graphiql/toolkit';
import "graphiql/graphiql.min.css";
import "./styles.css";


const fetcher = createGraphiQLFetcher({
  url: 'https://hasura.io/graphql', // TODO: update later
});

const GraphiQLIDE = ({ query, variables, response, viewOnly=true }) => (
  <BrowserOnly>
    {() => (
      <div className={`graphiql ${cslx({'with-vars': !!variables, 'view-only': viewOnly})}`}>
        <GraphiQL
          readOnly={viewOnly}
          editorTheme={'dracula'}
          schema={null}
          fetcher={fetcher}
          query={query}
          variables={variables}
          response={response}
        />
      </div>
    )}
  </BrowserOnly>
);

export default GraphiQLIDE;