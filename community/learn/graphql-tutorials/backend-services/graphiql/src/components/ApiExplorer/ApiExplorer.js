import React, { Component } from 'react';
import PropTypes from 'prop-types';
import ApiRequestWrapper from './ApiRequestWrapper';
import Helmet from 'react-helmet';
import { push } from 'react-router-redux';

class ApiExplorer extends Component {
  componentWillMount() {
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
    let localStorageUrl;

    if (window.__env.graphqlEndpoint && window.__env.graphqlEndpoint !== 'undefined') {
      localStorageUrl = window.__env.graphqlEndpoint;
    } else {
      localStorageUrl = window.localStorage.getItem('ONLINE_GRAPHIQL_ENDPOINT');
    }

    const styles = require('./ApiExplorer.scss');

    const wrapperClass = styles.apiExplorerWrapper;
    const requestStyles = '';
    const wdClass = '';

    const requestWrapper = (
      <ApiRequestWrapper
        credentials={this.props.credentials}
        explorerData={this.props.explorerData}
        details={this.props.displayedApi.details}
        request={this.props.displayedApi.request}
        requestStyles={requestStyles}
        dispatch={this.props.dispatch}
        wdStyles={wdClass}
        route={this.props.route}
        dataHeaders={this.props.dataHeaders}
        headerFocus={this.props.headerFocus}
        queryParams={this.props.location.query}
        graphqlEndpoint={localStorageUrl}
        auth={this.props.route.auth}
      />
    );

    return (
      <div className={'container-fluid ' + styles.padd_remove}>
        <Helmet
          title="GraphiQL Online with Headers | Built by Hasura"
          description="An online version of GraphiQL. Manage headers easily. Test your GraphQL servers."
        />
        <div className={wrapperClass}>{requestWrapper}</div>
      </div>
    );
  }
}

ApiExplorer.propTypes = {
  modalState: PropTypes.object.isRequired,
  dispatch: PropTypes.func.isRequired,
  route: PropTypes.object.isRequired,
  headerFocus: PropTypes.bool.isRequired,
  location: PropTypes.object.isRequired,
};

export default ApiExplorer;
