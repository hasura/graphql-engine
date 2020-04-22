import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Helmet from 'react-helmet';

import ApiRequestWrapper from './ApiRequestWrapper';

import globals from '../../../Globals';

class ApiExplorer extends Component {
  render() {
    const {
      displayedApi,
      credentials,
      explorerData,
      route,
      dataHeaders,
      headerFocus,
      trackedTableCount,
      location,
      serverConfig,
    } = this.props;

    const styles = require('./ApiExplorer.scss');
    const consoleUrl =
      window.location.protocol +
      '//' +
      window.location.host +
      globals.urlPrefix;

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
            numberOfTables={trackedTableCount}
            headerFocus={headerFocus}
            urlParams={location.query}
            serverVersion={globals.serverVersion}
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
  trackedTableCount: PropTypes.number.isRequired,
  location: PropTypes.object.isRequired,
};

const generatedApiExplorer = connect => {
  const mapStateToProps = state => {
    return {
      ...state.apiexplorer,
      credentials: {},
      dataApiExplorerData: { ...state.dataApiExplorer },
      dataHeaders: state.tables.dataHeaders,
      tables: state.tables.allSchemas,
      serverConfig: state.main.serverConfig ? state.main.serverConfig.data : {},
    };
  };
  return connect(mapStateToProps)(ApiExplorer);
};

export default generatedApiExplorer;
