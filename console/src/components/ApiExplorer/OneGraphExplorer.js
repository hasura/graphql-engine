import React from 'react';

import { getIntrospectionQuery, buildClientSchema } from 'graphql';
import { getHeadersAsJSON } from './utils';
import GraphiQLExplorer from 'graphiql-explorer-hasura';
import './GraphiQL.css';
import './OneGraphExplorer.css';
import { makeDefaultArg, getDefaultScalarArgValue } from './onegraphUtils';

class OneGraphExplorer extends React.Component {
  state = {
    explorerOpen: false,
    explorerWidth: 350,
    schema: null,
    query: this.props.query,
  };

  componentDidMount() {
    const { endpoint, headers } = this.props;
    fetch(endpoint, {
      method: 'POST',
      headers: getHeadersAsJSON(headers || []),
      body: JSON.stringify({
        query: getIntrospectionQuery(),
      }),
    })
      .then(response => response.json())
      .then(result => {
        this.setState({
          schema: buildClientSchema(result.data),
        });
      });
  }

  toggleExplorer = () => {
    this.setState(state => ({
      explorerOpen: !state.explorerOpen,
    }));
  };

  editQuery = query => {
    this.setState({ query });
  };

  render() {
    const { schema, explorerOpen, query } = this.state;
    const { renderGraphiql } = this.props;
    return (
      <div className="graphiql-container">
        <div className="gqlexplorer">
          <GraphiQLExplorer
            schema={schema}
            query={query}
            onEdit={this.editQuery}
            explorerIsOpen={explorerOpen}
            onToggleExplorer={this.toggleExplorer}
            getDefaultScalarArgValue={getDefaultScalarArgValue}
            makeDefaultArg={makeDefaultArg}
          />
        </div>
        <div className="explorerGraphiqlSeparator" />
        {renderGraphiql({
          query,
          onEditQuery: this.editQuery,
          toggleExplorer: this.toggleExplorer,
        })}
      </div>
    );
  }
}

export default OneGraphExplorer;
