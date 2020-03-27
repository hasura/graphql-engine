import React from 'react';
import PropTypes from 'prop-types';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';

import {
  inputChange,
  UPDATE_FORWARD_CLIENT_HEADERS,
} from '../Add/addRemoteSchemaReducer';

import CommonHeader from '../../../Common/Layout/ReusableHeader/Header';
import { Icon } from '../../../UIKit/atoms';
import DropdownButton from '../../../Common/DropdownButton/DropdownButton';
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
    headers,
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
    graphqlurl: (
      <Tooltip id="tooltip-cascade">
        Remote GraphQL server’s URL. E.g. https://my-domain/v1/graphql
      </Tooltip>
    ),
    clientHeaderForward: (
      <Tooltip id="tooltip-cascade">
        Toggle forwarding headers sent by the client app in the request to your
        remote GraphQL server
      </Tooltip>
    ),
    additionalHeaders: (
      <Tooltip id="tooltip-cascade">
        Custom headers to be sent to the remote GraphQL server
      </Tooltip>
    ),
    schema: (
      <Tooltip id="tooltip-cascade">
        Give this GraphQL schema a friendly name.
      </Tooltip>
    ),
    timeoutConf: (
      <Tooltip id="tooltip-cascade">
        Configure timeout for your remote GraphQL server. Defaults to 60
        seconds.
      </Tooltip>
    ),
  };

  const getTimeoutSection = () => {
    return (
      <React.Fragment>
        <div className={styles.subheading_text}>
          GraphQL server timeout
          <OverlayTrigger placement="right" overlay={tooltips.timeoutConf}>
            <Icon type="questionCircle" size={12} ml="xs" />
          </OverlayTrigger>
        </div>
        <label
          className={
            styles.inputLabel + ' radio-inline ' + styles.padd_left_remove
          }
        >
          <input
            className={'form-control'}
            type="text"
            placeholder="Timeout in seconds"
            value={timeoutConf}
            data-key="timeoutConf"
            onChange={handleInputChange}
            disabled={isDisabled}
            data-test="remote-schema-timeout-conf"
            pattern="^\d+$"
            title="Only non negative integers are allowed"
          />
        </label>
      </React.Fragment>
    );
  };

  return (
    <div className={styles.CommonWrapper}>
      <div className={styles.subheading_text + ' ' + styles.addPaddTop}>
        Remote Schema name *
        <OverlayTrigger placement="right" overlay={tooltips.schema}>
          <Icon type="questionCircle" size={12} ml="xs" />
        </OverlayTrigger>
      </div>
      <label
        className={
          styles.inputLabel + ' radio-inline ' + styles.padd_left_remove
        }
      >
        <input
          className={'form-control'}
          type="text"
          placeholder="Name of the schema"
          value={name}
          data-key="name"
          onChange={handleInputChange}
          disabled={isDisabled}
          required
          data-test="remote-schema-schema-name"
          pattern="^[a-zA-Z0-9-_]*$"
          title="Special characters except '-' or '_' are not allowed"
        />
      </label>
      <hr />
      <div className={styles.subheading_text}>
        GraphQL server URL *
        <OverlayTrigger placement="right" overlay={tooltips.graphqlurl}>
          <Icon type="questionCircle" size={12} ml="xs" />
        </OverlayTrigger>
      </div>
      <div className={styles.wd_300}>
        <DropdownButton
          dropdownOptions={[
            { display_text: 'URL', value: 'manualUrl' },
            { display_text: 'From env var', value: 'envName' },
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
          id="graphql-server-url"
          inputPlaceHolder={
            (manualUrl !== null && 'https://my-graphql-service.com/graphql') ||
            (envName !== null && 'MY_GRAPHQL_ENDPOINT')
          }
          testId="remote-schema-graphql-url"
        />
      </div>
      <br />
      <small>
        Note: Specifying the server URL via an environmental variable is
        recommended if you have different URLs for multiple environments.
      </small>
      <div className={styles.subheading_text + ' ' + styles.addPaddTop}>
        Headers for the remote GraphQL server
      </div>
      <div className={styles.check_box}>
        <label>
          <input
            onChange={toggleForwardHeaders}
            className={styles.display_inline + ' ' + styles.add_mar_right}
            type="checkbox"
            value="forwardHeaders"
            data-test="forward-remote-schema-headers"
            checked={forwardClientHeaders}
            disabled={isDisabled}
          />
          <span>Forward all headers from client</span>
        </label>
        <OverlayTrigger
          placement="right"
          overlay={tooltips.clientHeaderForward}
        >
          <Icon type="questionCircle" size={12} ml="xs" />
        </OverlayTrigger>
      </div>
      <div className={styles.subheading_text + ' ' + styles.font_normal}>
        Additional headers:
        <OverlayTrigger placement="right" overlay={tooltips.additionalHeaders}>
          <Icon type="questionCircle" size={12} ml="xs" />
        </OverlayTrigger>
      </div>
      <CommonHeader
        eventPrefix="REMOTE_SCHEMA"
        headers={headers}
        dispatch={dispatch}
        typeOptions={[
          { display_text: 'Value', value: 'static' },
          { display_text: 'From env var', value: 'env' },
        ]}
        isDisabled={isDisabled}
        placeHolderText={getPlaceHolderText}
        keyInputPlaceholder="header name"
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
  dispatch: PropTypes.func.isRequired,
};

export default Common;
