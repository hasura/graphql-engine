import React from 'react';

const AllowedQueriesNotes: React.FC = () => {
  return (
    <div>
      <div>
        If GraphQL Engine is started with the{' '}
        <code>HASURA_GRAPHQL_ENABLE_ALLOWLIST</code> env var or the{' '}
        <code>--enable-allowlist</code> flag set to <i>true</i>, only operations
        added to the allow-list will be allowed to be executed.&nbsp;
        <a
          href="https://hasura.io/docs/latest/graphql/core/deployment/allow-list.html"
          target="_blank"
          rel="noopener noreferrer"
        >
          <i>(Read more)</i>
        </a>
      </div>
      <div className="mt-md">
        <b>Notes</b>
        <div className="pl-lg pt-sm">
          All allowed operations need to have a unique name for reference
        </div>
      </div>
    </div>
  );
};

export default AllowedQueriesNotes;
