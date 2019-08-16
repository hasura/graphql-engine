import React, { Component } from 'react';
import PropTypes from 'prop-types';
import ApiRequest from './ApiRequest/ApiRequest';
import ApiResponse from './ApiResponse/ApiResponse';
import ApiRequestDetails from './ApiRequest/ApiRequestDetails';

class ApiRequestWrapper extends Component {
  render() {
    const styles = require('./ApiExplorer.scss');

    const getAPIRequestDetailsSection = () => {
       return (
         <ApiRequestDetails
           title={this.props.details.title}
           description={this.props.details.description}
         />
       );

      return null;
    };

    return (
      <div
        id="apiRequestBlock"
        className={
          styles.padd_left +
          ' ' +
          styles.padd_right +
          ' ' +
          styles.ApiRequestWrapperVH
        }
      >
        {getAPIRequestDetailsSection()}

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
          urlParams={this.props.urlParams}
          serverVersion={this.props.serverVersion}
          consoleUrl={this.props.consoleUrl}
          serverConfig={this.props.serverConfig}
          queryParams={this.props.queryParams}
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
  dispatch: PropTypes.func,
  numberOfTables: PropTypes.number,
  headerFocus: PropTypes.bool.isRequired,
  urlParams: PropTypes.bool.isRequired,
  consoleUrl: PropTypes.string.isRequired,
};

export default ApiRequestWrapper;
