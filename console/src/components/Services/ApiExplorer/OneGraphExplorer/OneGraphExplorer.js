import React from 'react';
import { getIntrospectionQuery, buildClientSchema } from 'graphql';
import GraphiQLExplorer from 'graphiql-explorer';
import { setLoading } from '../Actions';

import {
  makeDefaultArg,
  getDefaultScalarArgValue,
  getExplorerWidth,
  setExplorerWidth,
  getExplorerIsOpen,
  setExplorerIsOpen,
} from './utils';
import { getGraphiQLQueryFromLocalStorage } from '../GraphiQLWrapper/utils';
import { getRemoteQueries } from '../Actions';
import { getHeadersAsJSON } from '../utils';

import '../GraphiQLWrapper/GraphiQL.css';
import './OneGraphExplorer.css';
import { showErrorNotification } from '../../Common/Notification';

class OneGraphExplorer extends React.Component {
  state = {
    explorerOpen: getExplorerIsOpen(),
    explorerWidth: getExplorerWidth(),
    explorerClientX: null,
    schema: null,
    query: undefined,
    isResizing: false,
    previousIntrospectionHeaders: [],
  };

  componentDidMount() {
    this.setPersistedQuery();
    this.introspect();
  }

  componentDidUpdate(prevProps) {
    const { headerFocus, headers, loading } = this.props;
    const { previousIntrospectionHeaders } = this.state;
    // always introspect if mode changes
    if (this.props.mode !== prevProps.mode) {
      this.introspect();
      return;
    }
    if (!headerFocus && !loading) {
      if (
        JSON.stringify(headers) !== JSON.stringify(previousIntrospectionHeaders)
      ) {
        this.introspect();
      }
    }
  }

  setPersistedQuery() {
    const { urlParams, numberOfTables } = this.props;

    const queryFile = urlParams ? urlParams.query_file : null;

    if (queryFile) {
      getRemoteQueries(queryFile, remoteQuery =>
        this.setState({ query: remoteQuery })
      );
    } else if (numberOfTables === 0) {
      const NO_TABLES_MESSAGE = `# Looks like you do not have any tables.
# Click on the "Data" tab on top to create tables
# Try out GraphQL queries here after you create tables
`;

      this.setState({ query: NO_TABLES_MESSAGE });
    } else {
      const localStorageQuery = getGraphiQLQueryFromLocalStorage();

      if (localStorageQuery) {
        if (localStorageQuery.includes('do not have')) {
          const FRESH_GRAPHQL_MSG = '# Try out GraphQL queries here\n';

          this.setState({ query: FRESH_GRAPHQL_MSG });
        } else {
          this.setState({ query: localStorageQuery });
        }
      }
    }
  }

  introspect() {
    const {
      endpoint,
      headersInitialised,
      headers: headers_,
      dispatch,
    } = this.props;
    if (!headersInitialised) {
      return;
    }
    const headers = JSON.parse(JSON.stringify(headers_));
    dispatch(setLoading(true));
    this.setState({ schema: null });
    fetch(endpoint, {
      method: 'POST',
      headers: getHeadersAsJSON(headers || []),
      body: JSON.stringify({
        query: getIntrospectionQuery(),
      }),
    })
      .then(response => response.json())
      .then(result => {
        if (result.errors && result.errors.length > 0) {
          const errorMessage = result.errors[0].message;
          dispatch(
            showErrorNotification(
              'Schema introspection query failed',
              errorMessage
            )
          );
          this.setState({
            schema: null,
            previousIntrospectionHeaders: headers,
          });
          return;
        }
        this.setState({
          schema: buildClientSchema(result.data),
          previousIntrospectionHeaders: headers,
        });
      })
      .catch(() => {
        this.setState({
          schema: null,
          previousIntrospectionHeaders: headers,
        });
      })
      .finally(() => {
        dispatch(setLoading(false));
      });
  }

  onExplorerResize = e => {
    const { explorerClientX, explorerWidth } = this.state;

    if (explorerClientX === null) {
      this.setState({ explorerClientX: e.clientX });
    } else {
      const newExplorerWidth = explorerWidth + e.clientX - explorerClientX;

      setExplorerWidth(newExplorerWidth);

      this.setState({
        explorerWidth: newExplorerWidth,
        explorerClientX: e.clientX,
      });
    }
  };

  editQuery = query => {
    this.setState({ query });
  };

  handleToggle = () => {
    const newIsOpen = !this.state.explorerOpen;

    setExplorerIsOpen(newIsOpen);

    this.setState({ explorerOpen: newIsOpen });
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

    const explorer = (
      <GraphiQLExplorer
        schema={schema}
        query={query}
        onEdit={this.editQuery}
        explorerIsOpen={explorerOpen}
        onToggleExplorer={this.handleToggle}
        getDefaultScalarArgValue={getDefaultScalarArgValue}
        makeDefaultArg={makeDefaultArg}
        width={explorerWidth}
      />
    );

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
      query: query,
      onEditQuery: this.editQuery,
      schema: schema,
      toggleExplorer: this.handleToggle,
    });

    return (
      <div
        className={
          'graphiql-container' + (isResizing ? ' explorerCursorResize' : '')
        }
        onMouseUp={this.handleExplorerResizeStop}
      >
        <div className="gqlexplorer">
          {explorer}
          {explorerSeparator}
        </div>
        {graphiql}
      </div>
    );
  }
}

export default OneGraphExplorer;
