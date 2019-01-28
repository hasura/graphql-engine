/* eslint-disable */
import React, { Component } from 'react';
import PropTypes from 'prop-types';
import ApiCollectionPanel from './ApiCollectionPanel';
import ApiRequestWrapper from './ApiRequestWrapper';
import Helmet from 'react-helmet';

import {
  changeTabSelection,
  changeApiSelection,
  expandAuthApi,
  clearHistory,
  changeRequestParams,
} from './Actions';

// import {triggerOnBoarding} from '../Main/Actions';

class ApiExplorer extends Component {
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

  render() {
    const styles = require('./ApiExplorer.scss');
    let wrapperClass = styles.apiExplorerWrapper;
    let panelStyles = '';
    let requestStyles = '';
    let wdClass = '';
    // check if onboarding is enabled

    // show api request wrapper or graphiql depending on selection
    const displayedApi = this.props.displayedApi;
    let requestWrapper = (
      <ApiRequestWrapper
        credentials={this.props.credentials}
        explorerData={this.props.explorerData}
        details={displayedApi.details}
        request={displayedApi.request}
        requestStyles={requestStyles}
        dispatch={this.props.dispatch}
        wdStyles={wdClass}
        route={this.props.route}
        dataHeaders={this.props.dataHeaders}
        numberOfTables={this.props.tables.length}
        headerFocus={this.props.headerFocus}
        queryParams={this.props.location.query}
        serverVersion={this.props.serverVersion}
      />
    );

    return (
      <div className={'container-fluid ' + styles.padd_remove}>
        <Helmet title="API Explorer | Hasura" />
        <div className={wrapperClass}>{requestWrapper}</div>
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
