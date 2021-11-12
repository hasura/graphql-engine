import React from 'react';
import GraphiQL from 'graphiql';
import cslx from 'clsx';
import { createGraphiQLFetcher } from '@graphiql/toolkit';
import "graphiql/graphiql.min.css";
import "./styles.css";


const fetcher = createGraphiQLFetcher({
  url: window.location.origin + '/graphql',
});

const GraphiQLIDE = ({ query, variables, response, viewOnly=true }) => (
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
);

export default GraphiQLIDE;