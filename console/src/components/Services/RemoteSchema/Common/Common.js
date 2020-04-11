import React from 'react';
import PropTypes from 'prop-types';

import {
  inputChange,
  UPDATE_FORWARD_CLIENT_HEADERS
} from '../Add/addRemoteSchemaReducer';
import CommonHeader from '../../../Common/Layout/ReusableHeader/Header';
import DropdownButton from '../../../Common/DropdownButton/DropdownButton';
import { ToolTip, Heading } from '../../../UIKit/atoms';
import styles from '../RemoteSchema.scss';

const Common = props => {
  const {
    name,
    manualUrl,
    envName,
    timeoutConf,
    forwardClientHeaders,
    dispatch,
    editState,
    headers
  } = props;

  const { isModify, id } = editState;

  const isDisabled = id >= 0 && !isModify;
  const urlRequired = !manualUrl && !envName;

  const getPlaceHolderText = valType => {
    if (valType === 'static') {
      return 'header value';
    }
    return 'env var name';
  };

  const handleInputChange = e => {
    const fieldName = e.target.getAttribute('data-key');
    dispatch(inputChange(fieldName, e.target.value));
  };

  const toggleUrlParam = e => {
    const field = e.target.getAttribute('value');
    dispatch(inputChange(field, ''));
  };

  const toggleForwardHeaders = () => {
    dispatch({ type: UPDATE_FORWARD_CLIENT_HEADERS });
  };

  const tooltips = {
    graphqlurl:
      'Remote GraphQL server’s URL. E.g. https://my-domain/v1/graphql',
    clientHeaderForward:
      'Toggle forwarding headers sent by the client app in the request to your remote GraphQL server',
    additionalHeaders: 'Custom headers to be sent to the remote GraphQL server',
    schema: 'Give this GraphQL schema a friendly name.',
    timeoutConf:
      'Configure timeout for your remote GraphQL server. Defaults to 60 seconds.'
  };

  const getTimeoutSection = () => {
    return (
      <React.Fragment>
        <Heading type='subHeading'>
          GraphQL server timeout
          <ToolTip message={tooltips.timeoutConf} ml='sm' />
        </Heading>
        <label
          className={
            styles.inputLabel + ' radio-inline ' + styles.padd_left_remove
          }
        >
          <input
            className={'form-control'}
            type='text'
            placeholder='Timeout in seconds'
            value={timeoutConf}
            data-key='timeoutConf'
            onChange={handleInputChange}
            disabled={isDisabled}
            data-test='remote-schema-timeout-conf'
            pattern='^\d+$'
            title='Only non negative integers are allowed'
          />
        </label>
      </React.Fragment>
    );
  };

  return (
    <div className={styles.CommonWrapper}>
      <Heading type='subHeading' pt='20px'>
        Remote Schema name *
        <ToolTip message={tooltips.schema} ml='sm' />
      </Heading>
      <label
        className={
          styles.inputLabel + ' radio-inline ' + styles.padd_left_remove
        }
      >
        <input
          className={'form-control'}
          type='text'
          placeholder='Name of the schema'
          value={name}
          data-key='name'
          onChange={handleInputChange}
          disabled={isDisabled}
          required
          data-test='remote-schema-schema-name'
          pattern='^[a-zA-Z0-9-_]*$'
          title="Special characters except '-' or '_' are not allowed"
        />
      </label>
      <hr />
      <Heading type='subHeading'>
        GraphQL server URL *
        <ToolTip message={tooltips.graphqlurl} ml='sm' />
      </Heading>
      <div className={styles.wd_300}>
        <DropdownButton
          dropdownOptions={[
            { display_text: 'URL', value: 'manualUrl' },
            { display_text: 'From env var', value: 'envName' }
          ]}
          title={
            (manualUrl !== null && 'URL') ||
            (envName !== null && 'From env var') ||
            'Value'
          }
          dataKey={
            (manualUrl !== null && 'manualUrl') ||
            (envName !== null && 'envName')
          }
          onButtonChange={toggleUrlParam}
          onInputChange={handleInputChange}
          required={urlRequired}
          bsClass={styles.dropdown_button}
          inputVal={manualUrl || envName}
          disabled={isDisabled}
          id='graphql-server-url'
          inputPlaceHolder={
            (manualUrl !== null && 'https://my-graphql-service.com/graphql') ||
            (envName !== null && 'MY_GRAPHQL_ENDPOINT')
          }
          testId='remote-schema-graphql-url'
        />
      </div>
      <br />
      <small>
        Note: Specifying the server URL via an environmental variable is
        recommended if you have different URLs for multiple environments.
      </small>
      <Heading type='subHeading' pt='20px'>
        Headers for the remote GraphQL server
      </Heading>
      <div className={styles.check_box}>
        <label>
          <input
            onChange={toggleForwardHeaders}
            className={styles.display_inline + ' ' + styles.add_mar_right}
            type='checkbox'
            value='forwardHeaders'
            data-test='forward-remote-schema-headers'
            checked={forwardClientHeaders}
            disabled={isDisabled}
          />
          <span>Forward all headers from client</span>
        </label>
        <ToolTip message={tooltips.clientHeaderForward} />
      </div>
      <Heading type='subHeading' fontWeight='normal'>
        Additional headers:
        <ToolTip message={tooltips.additionalHeaders} ml='sm' />
      </Heading>
      <CommonHeader
        eventPrefix='REMOTE_SCHEMA'
        headers={headers}
        dispatch={dispatch}
        typeOptions={[
          { display_text: 'Value', value: 'static' },
          { display_text: 'From env var', value: 'env' }
        ]}
        isDisabled={isDisabled}
        placeHolderText={getPlaceHolderText}
        keyInputPlaceholder='header name'
      />
      <hr />
      {getTimeoutSection()}
    </div>
  );
};

Common.propTypes = {
  name: PropTypes.string.isRequired,
  envName: PropTypes.string.isRequired,
  manualUrl: PropTypes.string.isRequired,
  headers: PropTypes.array.isRequired,
  forwardClientHeaders: PropTypes.bool.isRequired,
  dispatch: PropTypes.func.isRequired
};

export default Common;
