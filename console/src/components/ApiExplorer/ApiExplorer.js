import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Helmet from 'react-helmet';

import ApiRequestWrapper from './ApiRequestWrapper';
// import ApiCollectionPanel from './ApiCollectionPanel';

// import {
//   changeTabSelection,
//   changeApiSelection,
//   expandAuthApi,
//   clearHistory,
//   // changeRequestParams,
// } from './Actions';

// import {triggerOnBoarding} from '../Main/Actions';

class ApiExplorer extends Component {
  componentWillUnmount() {
    this.clearCodeMirrorHints();
  }

  clearCodeMirrorHints() {
    const cmNodes = document.querySelectorAll('.CodeMirror-hints.graphiql');

    if (cmNodes.length > 0) {
      cmNodes.forEach(cm => {
        cm.remove();
      });
    }
  }

  // onTabSelectionChanged = tabIndex => {
  //   this.props.dispatch(changeTabSelection(tabIndex));
  // };
  //
  // onApiSelectionChanged = (selectedApi, authApiExpanded) => {
  //   this.props.dispatch(changeApiSelection(selectedApi, authApiExpanded));
  // };
  //
  // onAuthApiExpanded = index => {
  //   this.props.dispatch(expandAuthApi(index));
  // };
  //
  // onClearHistoryClicked = () => {
  //   this.props.dispatch(clearHistory());
  // };
  //
  // getDQBQuery(propsObj) {
  //   const { type, args } = propsObj;
  //   const _query = {};
  //   _query.type = type;
  //   _query.args = JSON.parse(JSON.stringify(args));
  //   return _query;
  // }
  //
  // updateDQBState(data) {
  //   this.props.dispatch(hydrateDQBData(data));
  // }

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
    } = this.props;

    const styles = require('./ApiExplorer.scss');

    const wrapperClass = styles.apiExplorerWrapper;
    const requestStyles = '';
    const wdClass = '';

    // show api request wrapper or graphiql depending on selection
    const requestWrapper = (
      <ApiRequestWrapper
        credentials={credentials}
        explorerData={explorerData}
        details={displayedApi.details}
        request={displayedApi.request}
        requestStyles={requestStyles}
        dispatch={this.props.dispatch}
        wdStyles={wdClass}
        route={route}
        dataHeaders={dataHeaders}
        numberOfTables={tables.length}
        headerFocus={headerFocus}
        queryParams={location.query}
        serverVersion={serverVersion}
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
