import React from 'react';
import GraphiQL from 'graphiql';
import cslx from 'clsx';
import 'graphiql/graphiql.min.css';
import './styles.scss';

const GraphiQLIDE = ({ query, variables, response, viewOnly = true }) => {
  const notReal = async ({ query }) => {
    return {
      data: {
        easterEgg: `This query and response is for demo purposes only. Running it doesn't actually hit an API. Refresh the page to see the original response.`,
      },
    };
  };

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
        fetcher={notReal}
        query={query}
        variables={variables}
        response={response}
      />
    </div>
  );
};

export default GraphiQLIDE;
