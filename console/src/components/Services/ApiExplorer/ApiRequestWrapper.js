import React from 'react';
import PropTypes from 'prop-types';

import ApiRequest from './ApiRequest/ApiRequest';
import ApiResponse from './ApiResponse/ApiResponse';
// import ApiRequestDetails from './ApiRequestDetails';
import styles from './ApiExplorer.scss';

const ApiRequestWrapper = props => {
  const { bodyType, request, details, explorerData } = props;

  const getAPIRequestDetailsSection = () => {
    // return (
    //   <ApiRequestDetails
    //     title={details.title}
    //     description={details.description}
    //   />
    // );

    return null;
  };

  const getAPIResponseSection = () => {
    let apiResponseSection = null;

    if (bodyType !== 'graphql') {
      apiResponseSection = (
        <ApiResponse
          {...props.explorerData}
          categoryType={details.category}
          showHelpBulb={request.showHelpBulb ? request.showHelpBulb : false}
          url={request.url}
        />
      );
    }

    return apiResponseSection;
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
        bodyType={request.bodyType ? request.bodyType : ''}
        credentials={props.credentials}
        method={request.method}
        url={request.url}
        headers={request.headers}
        headersInitialised={request.headersInitialised}
        params={request.params}
        explorerData={explorerData}
        dispatch={props.dispatch}
        dataHeaders={props.dataHeaders}
        numberOfTables={props.numberOfTables}
        headerFocus={props.headerFocus}
        urlParams={props.urlParams}
        serverVersion={props.serverVersion}
        consoleUrl={props.consoleUrl}
        serverConfig={props.serverConfig}
      />

      {getAPIResponseSection()}
    </div>
  );
};

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
