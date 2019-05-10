import React from 'react';

class AllowedQueriesNotes extends React.Component {
  render() {
    return (
      <div>
        If GraphQL Engine is started with the{' '}
        <code>HASURA_GRAPHQL_ENABLE_ALLOWLIST</code> env var or the{' '}
        <code>--enable-allowlist</code> flag set to <i>true</i>, only queries
        added to the allow-list will be allowed to be executed
      </div>
    );
  }
}

export default AllowedQueriesNotes;
