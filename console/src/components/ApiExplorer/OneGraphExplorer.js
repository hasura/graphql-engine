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

class OneGraphExplorer extends React.Component {
  state = {
    explorerOpen: false,
    explorerWidth: getExplorerWidthFromLocalStorage(),
    explorerClientX: null,
    schema: null,
    query: this.props.query,
    isResizing: false,
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
    return (
      <div
        className={`graphiql-container ${
          isResizing ? 'explorerCursorResize' : ''
        }`}
        onMouseUp={this.handleExplorerResizeStop}
      >
        <div className="gqlexplorer">
          {explorerOpen && (
            <div
              className="explorerGraphiqlSeparator explorerCursorResize"
              onMouseDown={this.handleExplorerResize}
              onMouseUp={this.handleExplorerResizeStop}
            />
          )}
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
