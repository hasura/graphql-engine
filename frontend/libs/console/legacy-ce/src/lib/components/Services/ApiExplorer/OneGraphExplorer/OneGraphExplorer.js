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
import Spinner from '../../../Common/Spinner/Spinner';
import {
  showErrorNotification,
  showWarningNotification,
} from '../../Common/Notification';
import requestAction from '../../../../utils/requestAction';

class OneGraphExplorer extends React.Component {
  state = {
    explorerOpen: getExplorerIsOpen(),
    explorerWidth: getExplorerWidth(),
    explorerClientX: null,
    schema: null,
    query: this.props.query || '',
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

    // introspect by force if toggled through Redux
    if (
      this.props.forceIntrospectAt &&
      this.props.forceIntrospectAt != prevProps.forceIntrospectAt
    ) {
      this.introspect();
      return;
    }

    // set query in graphiql through Redux
    if (this.props.query != prevProps.query) {
      // eslint-disable-next-line react/no-did-update-set-state
      this.setState({ query: this.props.query });
    }
  }

  setPersistedQuery() {
    const { urlParams, numberOfTables, dispatch } = this.props;

    const queryFile = urlParams ? urlParams.query_file : null;
    const localStorageQuery = getGraphiQLQueryFromLocalStorage();
    if (queryFile) {
      getRemoteQueries(
        queryFile,
        remoteQuery => this.setState({ query: remoteQuery }),
        dispatch
      );
    } else if (numberOfTables === 0 && !localStorageQuery) {
      // when there are no tables and nothing in the localstorage, show the following comment in the graphiQL
      const NO_TABLES_MESSAGE = `# Looks like you do not have any tables.
# Click on the "Data" tab on top to create tables
# Try out GraphQL queries here after you create tables
`;
      // FIX ME : this message will be shown, whenever there are no tables tracked and nothing in history (LS),
      // there could still be a possibility when there are no tables but remote schemas even then this message will be shown only for the first time
      // after that when the user type something, the LS gets populated and this message will not be shown afterwards

      this.setState({ query: NO_TABLES_MESSAGE });
    } else {
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
    dispatch(
      requestAction(endpoint, {
        method: 'POST',
        headers: getHeadersAsJSON(headers || []),
        body: JSON.stringify({
          query: getIntrospectionQuery(),
        }),
      })
    )
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
        let clientSchema = null;
        try {
          clientSchema = buildClientSchema(result.data);
        } catch (err) {
          console.error(err);
          dispatch(
            showWarningNotification(
              `Failed to parse the schema`,
              `We are not able to render GraphiQL Explorer and Docs.
              You should still be able to try out your API from the GraphiQL Editor.`,
              null,
              <p className="pt-sm m-0">
                Please report an issue on our{' '}
                <a
                  target="_blank"
                  href="https://github.com/hasura/graphql-engine/issues/new"
                  rel="noopener noreferrer"
                >
                  GitHub
                </a>
                , so we can triage this and improve your experience.
              </p>
            )
          );
        }
        this.setState({
          schema: clientSchema,
          previousIntrospectionHeaders: headers,
        });
      })
      .catch(err => {
        dispatch(
          showErrorNotification(
            'Schema introspection query failed',
            err.message,
            err
          )
        );
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
    const { schema, explorerOpen, query, explorerWidth, isResizing } =
      this.state;

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

    if (!schema && this.props.loading) {
      return (
        <div className="h-full flex">
          <Spinner />
        </div>
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
          {this.props.loading ? (
            <div
              className={`h-full flex w-[${String(explorerWidth) || '300'}px]`}
            >
              <Spinner />
            </div>
          ) : (
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
          )}
          {explorerSeparator}
        </div>
        {graphiql}
      </div>
    );
  }
}

export default OneGraphExplorer;
