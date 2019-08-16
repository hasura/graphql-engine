import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Helmet from 'react-helmet';

import ApiRequestWrapper from './ApiRequestWrapper';

class ApiExplorer extends Component {
  componentDidMount() {
    let localStorageUrl;
    if (window.__env.graphqlEndpoint && window.__env.graphqlEndpoint !== 'undefined') {
      localStorageUrl = window.__env.graphqlEndpoint;
    } else {
      localStorageUrl = window.localStorage.getItem('ONLINE_GRAPHIQL_ENDPOINT');
    }
    if (!this.props.graphqlEndpoint && (localStorageUrl === 'undefined' || localStorageUrl === null)) {
      this.props.dispatch(push('/'));
    }
  }

  render() {
    const {
      displayedApi,
      credentials,
      explorerData,
      route,
      dataHeaders,
      tables,
      headerFocus,
      location,
      serverVersion,
      serverConfig,
    } = this.props;

    const styles = require('./ApiExplorer.scss');
    const consoleUrl = window.location.protocol + '//' + window.location.host;
      
    let localStorageUrl;
    if (window.__env.graphqlEndpoint && window.__env.graphqlEndpoint !== 'undefined') {
      localStorageUrl = window.__env.graphqlEndpoint;
    } else {
      localStorageUrl = window.localStorage.getItem('ONLINE_GRAPHIQL_ENDPOINT');
    }

    return (
      <div className={'container-fluid ' + styles.padd_remove}>
        <Helmet title="API Explorer | Hasura" />
        <div className={styles.apiExplorerWrapper}>
          <ApiRequestWrapper
            dispatch={this.props.dispatch}
            credentials={credentials}
            explorerData={explorerData}
            details={displayedApi.details}
            request={displayedApi.request}
            route={route}
            dataHeaders={dataHeaders}
            numberOfTables={0}
            headerFocus={headerFocus}
            queryParams={this.props.location.query}
            graphqlEndpoint={localStorageUrl}
            urlParams={location.query}
            serverVersion={serverVersion}
            consoleUrl={consoleUrl}
            serverConfig={serverConfig}
          />
        </div>
      </div>
    );
  }
}

ApiExplorer.propTypes = {
  modalState: PropTypes.object.isRequired,
  displayedApi: PropTypes.object.isRequired,
  dispatch: PropTypes.func.isRequired,
  route: PropTypes.object.isRequired,
  tables: PropTypes.array.isRequired,
  headerFocus: PropTypes.bool.isRequired,
  location: PropTypes.object.isRequired,
};

export default ApiExplorer;
