import React from 'react';
import PropTypes from 'prop-types';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';
import DropdownButton from '../../../Common/DropdownButton/DropdownButton';

import {
  inputChange,
  UPDATE_FORWARD_CLIENT_HEADERS,
} from '../Add/addRemoteSchemaReducer';

import CommonHeader from '../../../Common/Layout/ReusableHeader/Header';

class Common extends React.Component {
  getPlaceHolderText(valType) {
    if (valType === 'static') {
      return 'header value';
    }
    return 'env var name';
  }

  handleInputChange(e) {
    const fieldName = e.target.getAttribute('data-key');
    this.props.dispatch(inputChange(fieldName, e.target.value));
  }

  toggleUrlParam(e) {
    const field = e.target.getAttribute('value');
    this.props.dispatch(inputChange(field, ''));
  }

  toggleForwardHeaders() {
    this.props.dispatch({ type: UPDATE_FORWARD_CLIENT_HEADERS });
  }

  render() {
    const styles = require('../RemoteSchema.scss');

    const {
      name,
      manualUrl,
      envName,
      timeoutConf,
      forwardClientHeaders,
    } = this.props;
    const { isModify, id } = this.props.editState;

    const isDisabled = id >= 0 && !isModify;
    const urlRequired = !manualUrl && !envName;

    const tooltips = {
      graphqlurl: (
        <Tooltip id="tooltip-cascade">
          Remote GraphQL server’s URL. E.g. https://my-domain/v1/graphql
        </Tooltip>
      ),
      clientHeaderForward: (
        <Tooltip id="tooltip-cascade">
          Toggle forwarding headers sent by the client app in the request to
          your remote GraphQL server
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
              <i className="fa fa-question-circle" aria-hidden="true" />
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
              onChange={this.handleInputChange.bind(this)}
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
            <i className="fa fa-question-circle" aria-hidden="true" />
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
            onChange={this.handleInputChange.bind(this)}
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
            <i className="fa fa-question-circle" aria-hidden="true" />
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
            onButtonChange={this.toggleUrlParam.bind(this)}
            onInputChange={this.handleInputChange.bind(this)}
            required={urlRequired}
            bsClass={styles.dropdown_button}
            inputVal={manualUrl || envName}
            disabled={isDisabled}
            id="graphql-server-url"
            inputPlaceHolder={
              (manualUrl !== null &&
                'https://my-graphql-service.com/graphql') ||
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
              onChange={this.toggleForwardHeaders.bind(this)}
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
            <i className="fa fa-question-circle" aria-hidden="true" />
          </OverlayTrigger>
        </div>
        <div className={styles.subheading_text + ' ' + styles.font_normal}>
          Additional headers:
          <OverlayTrigger
            placement="right"
            overlay={tooltips.additionalHeaders}
          >
            <i className="fa fa-question-circle" aria-hidden="true" />
          </OverlayTrigger>
        </div>
        <CommonHeader
          eventPrefix="REMOTE_SCHEMA"
          headers={this.props.headers}
          dispatch={this.props.dispatch}
          typeOptions={[
            { display_text: 'Value', value: 'static' },
            { display_text: 'From env var', value: 'env' },
          ]}
          isDisabled={isDisabled}
          placeHolderText={this.getPlaceHolderText.bind(this)}
          keyInputPlaceholder="header name"
        />
        <hr />
        {getTimeoutSection()}
      </div>
    );
  }
}

Common.propTypes = {
  name: PropTypes.string.isRequired,
  envName: PropTypes.string.isRequired,
  manualUrl: PropTypes.string.isRequired,
  headers: PropTypes.array.isRequired,
  forwardClientHeaders: PropTypes.bool.isRequired,
  dispatch: PropTypes.func.isRequired,
};

export default Common;
