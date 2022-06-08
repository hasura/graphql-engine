import React from 'react';
import GraphiQL from 'graphiql';
import useIsBrowser from '@docusaurus/useIsBrowser';
import cslx from 'clsx';
import { createGraphiQLFetcher } from '@graphiql/toolkit';
import 'graphiql/graphiql.min.css';
import './styles.scss';

const GraphiQLIDE = ({ query, variables, response, viewOnly = true }) => {
  const isBrowser = useIsBrowser();
  return (
    <div
      className={`graphiql ${cslx({
        'with-vars': !!variables,
        'view-only': viewOnly,
      })}`}
    >
      <GraphiQL
        readOnly={false}
        editorTheme={'dracula'}
        schema={null}
        fetcher={
          isBrowser
            ? createGraphiQLFetcher({
                url: 'https://hasura.io/graphql', // TODO: update later
              })
            : () => null
        }
        query={query}
        variables={variables}
        response={response}
      />
    </div>
  );
};

export default GraphiQLIDE;
