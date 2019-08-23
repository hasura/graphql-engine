import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Helmet from 'react-helmet';

import ApiRequestWrapper from './ApiRequestWrapper';

import globals from '../../../Globals';

/*
import ApiCollectionPanel from './ApiCollectionPanel';

import {
  changeTabSelection,
  changeApiSelection,
  expandAuthApi,
  clearHistory,
  // changeRequestParams,
} from './Actions';

import {triggerOnBoarding} from '../Main/Actions';
*/

class ApiExplorer extends Component {
  /*
  onTabSelectionChanged = tabIndex => {
    this.props.dispatch(changeTabSelection(tabIndex));
  };

  onApiSelectionChanged = (selectedApi, authApiExpanded) => {
    this.props.dispatch(changeApiSelection(selectedApi, authApiExpanded));
  };

  onAuthApiExpanded = index => {
    this.props.dispatch(expandAuthApi(index));
  };

  onClearHistoryClicked = () => {
    this.props.dispatch(clearHistory());
  };

  getDQBQuery(propsObj) {
    const { type, args } = propsObj;
    const _query = {};
    _query.type = type;
    _query.args = JSON.parse(JSON.stringify(args));
    return _query;
  }

  updateDQBState(data) {
    this.props.dispatch(hydrateDQBData(data));
  }
  */

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
            numberOfTables={tables.length}
            headerFocus={headerFocus}
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
