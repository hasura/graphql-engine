import React from 'react';
import { getIntrospectionQuery, buildClientSchema } from 'graphql';
import { getHeadersAsJSON } from './utils';
import GraphiQLExplorer from 'graphiql-explorer-hasura';
import './GraphiQL.css';
import './OneGraphExplorer.css';
import {
  makeDefaultArg,
  getDefaultScalarArgValue,
  getExplorerWidthFromLocalStorage,
  setExplorerWidthInLocalStorage,
} from './onegraphUtils';
import { getRemoteQueries } from './Actions';

class OneGraphExplorer extends React.Component {
  state = {
    explorerOpen: false,
    explorerWidth: getExplorerWidthFromLocalStorage(),
    explorerClientX: null,
    schema: null,
    query: this.props.query,
    isResizing: false,
    headers: this.props.headers || [],
  };

  componentDidMount() {
    this.introspect();
  }

  componentDidUpdate() {
    if (this.shouldIntrospect()) {
      this.introspect();
    }
  }

  setPersistedQuery() {
    const queryFile = this.props.queryParams
      ? this.props.queryParams.query_file
      : null;

    if (queryFile) {
      getRemoteQueries(queryFile, query => this.setState({ query }));
    } else {
      const NO_TABLES_MESSAGE =
        '# Looks like you do not have any tables.\n# Click on the "Data" tab on top to create tables\n# You can come back here and try out the GraphQL queries after you create tables\n';

      if (this.props.numberOfTables === 0) {
        this.setState({
          query: NO_TABLES_MESSAGE,
        });
      }
    }
  }

  shouldIntrospect() {
    return (
      JSON.stringify(this.props.headers) !== JSON.stringify(this.state.headers)
    );
  }

  introspect() {
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
          headers: JSON.parse(JSON.stringify(headers)),
        });
      })
      .catch(() => {
        this.setState({
          schema: null,
          headers: JSON.parse(JSON.stringify(headers)),
        });
      });
  }

  onExplorerResize = e => {
    const { explorerClientX, explorerWidth } = this.state;
    if (explorerClientX === null) {
      this.setState({ explorerClientX: e.clientX });
    } else {
      const newExplorerWidth = explorerWidth + e.clientX - explorerClientX;
      setExplorerWidthInLocalStorage(newExplorerWidth);
      this.setState({
        explorerWidth: newExplorerWidth,
        explorerClientX: e.clientX,
      });
    }
  };

  editQuery = query => {
    this.setState({ query });
  };

  toggleExplorer = () => {
    this.setState(state => ({
      explorerOpen: !state.explorerOpen,
    }));
  };

  handleExplorerResize = e => {
    e.preventDefault();
    document.addEventListener('mousemove', this.onExplorerResize);
    this.setState({
      isResizing: true,
    });
  };

  handleExplorerResizeStop = e => {
    e.preventDefault();
    document.removeEventListener('mousemove', this.onExplorerResize);
    this.setState({
      isResizing: false,
    });
  };

  render() {
    const {
      schema,
      explorerOpen,
      query,
      explorerWidth,
      isResizing,
    } = this.state;

    const { renderGraphiql } = this.props;

    let explorerSeparator;
    if (explorerOpen) {
      explorerSeparator = (
        <div
          className="explorerGraphiqlSeparator explorerCursorResize"
          onMouseDown={this.handleExplorerResize}
          onMouseUp={this.handleExplorerResizeStop}
        />
      );
    }

    const graphiql = renderGraphiql({
      query: query || undefined,
      onEditQuery: this.editQuery,
      schema: schema,
      toggleExplorer: this.toggleExplorer,
    });

    return (
      <div
        className={
          'graphiql-container' + (isResizing ? ' explorerCursorResize' : '')
        }
        onMouseUp={this.handleExplorerResizeStop}
      >
        <div className="gqlexplorer">
          {explorerSeparator}
          <GraphiQLExplorer
            schema={schema}
            query={query}
            onEdit={this.editQuery}
            explorerIsOpen={explorerOpen}
            onToggleExplorer={this.toggleExplorer}
            getDefaultScalarArgValue={getDefaultScalarArgValue}
            makeDefaultArg={makeDefaultArg}
            width={explorerWidth}
          />
        </div>
        {graphiql}
      </div>
    );
  }
}

export default OneGraphExplorer;
