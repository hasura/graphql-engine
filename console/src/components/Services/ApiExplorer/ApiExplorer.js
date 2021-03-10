import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Helmet from 'react-helmet';

import ApiRequestWrapper from './ApiRequestWrapper';

import globals from '../../../Globals';
import { getTables } from '../../../metadata/selector';

class ApiExplorer extends Component {
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
      mode,
      loading,
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
            mode={mode}
            dataHeaders={dataHeaders}
            numberOfTables={tables.length}
            headerFocus={headerFocus}
            urlParams={location.query}
            serverVersion={serverVersion}
            consoleUrl={consoleUrl}
            loading={loading}
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
  mode: PropTypes.string.isRequired,
};

const generatedApiExplorer = connect => {
  const mapStateToProps = state => {
    return {
      ...state.apiexplorer,
      serverVersion: state.main.serverVersion ? state.main.serverVersion : '',
      credentials: {},
      dataApiExplorerData: { ...state.dataApiExplorer },
      dataHeaders: state.tables.dataHeaders,
      tables: getTables(state),
      serverConfig: state.main.serverConfig ? state.main.serverConfig.data : {},
    };
  };
  return connect(mapStateToProps)(ApiExplorer);
};

export default generatedApiExplorer;
