import React, { Component } from 'react';
import PropTypes from 'prop-types';
import ApiRequest from './ApiRequest';
import ApiRequestDetails from './ApiRequestDetails';

class ApiRequestWrapper extends Component {
  render() {
    const styles = require('./ApiExplorer.scss');
    return (
      <div
        id="apiRequestBlock"
        className={
          this.props.wdStyles +
          ' ' +
          styles.padd_left +
          ' ' +
          styles.padd_right +
          ' ' +
          styles.ApiRequestWrapperVH +
          ' ' +
          this.props.requestStyles
        }
      >
        <ApiRequestDetails
          title={this.props.details.title}
          description={this.props.details.description}
        />
        <ApiRequest
          bodyType={
            this.props.request.bodyType ? this.props.request.bodyType : ''
          }
          credentials={this.props.credentials}
          method={this.props.request.method}
          url={this.props.graphqlEndpoint}
          headers={this.props.request.headers}
          params={this.props.request.params}
          explorerData={this.props.explorerData}
          dispatch={this.props.dispatch}
          dataHeaders={this.props.dataHeaders}
          numberOfTables={this.props.numberOfTables}
          headerFocus={this.props.headerFocus}
        />
      </div>
    );
  }
}

ApiRequestWrapper.propTypes = {
  details: PropTypes.object.isRequired,
  request: PropTypes.object.isRequired,
  explorerData: PropTypes.object.isRequired,
  credentials: PropTypes.object.isRequired,
  bodyType: PropTypes.string,
  showHelpBulb: PropTypes.bool,
  requestStyles: PropTypes.string,
  wdStyles: PropTypes.string,
  dispatch: PropTypes.func,
  numberOfTables: PropTypes.number,
  headerFocus: PropTypes.bool.isRequired,
  graphqlEndpoint: PropTypes.string,
};

export default ApiRequestWrapper;
