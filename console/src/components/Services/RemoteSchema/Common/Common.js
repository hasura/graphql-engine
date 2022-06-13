import React from 'react';
import PropTypes from 'prop-types';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';
import { FaQuestionCircle } from 'react-icons/fa';
import DropdownButton from '../../../Common/DropdownButton/DropdownButton';

import {
  inputChange,
  UPDATE_FORWARD_CLIENT_HEADERS,
} from '../Add/addRemoteSchemaReducer';

import CommonHeader from '../../../Common/Layout/ReusableHeader/Header';
import GraphQLCustomizationEdit from './GraphQLCustomization/GraphQLCustomizationEdit';

import { focusYellowRing, inputStyles, subHeading } from '../constants';

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

  handleCustomizationInputChange(updateValue) {
    this.props.dispatch(inputChange('customization', updateValue));
  }

  toggleUrlParam(e) {
    const field = e.target.getAttribute('value');
    this.props.dispatch(inputChange(field, ''));
  }

  toggleForwardHeaders() {
    this.props.dispatch({ type: UPDATE_FORWARD_CLIENT_HEADERS });
  }

  render() {
    const {
      name,
      manualUrl,
      envName,
      timeoutConf,
      forwardClientHeaders,
      comment,
      isNew = false,
      customization,
    } = this.props;

    const { isModify } = this.props.editState;

    const isDisabled = !isNew && !isModify;
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
      comment: (
        <Tooltip id="tooltip-cascade">
          A statement to help describe the remote schema in brief
        </Tooltip>
      ),
    };

    const getTimeoutSection = () => {
      return (
        <React.Fragment>
          <div className={subHeading}>
            GraphQL server timeout
            <OverlayTrigger placement="right" overlay={tooltips.timeoutConf}>
              <FaQuestionCircle aria-hidden="true" className="mb-1 ml-xs" />
            </OverlayTrigger>
          </div>
          <label className="ml-0 pl-0">
            <input
              className={`${inputStyles} w-72`}
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
      <div>
        <div className={`${subHeading} pt-md`}>
          Remote Schema name *
          <OverlayTrigger placement="right" overlay={tooltips.schema}>
            <FaQuestionCircle aria-hidden="true" className="mb-1 ml-xs" />
          </OverlayTrigger>
        </div>
        <label className="ml-0 pl-0 w-80">
          <input
            className={`${inputStyles} disabled:bg-[#EFEFEF]`}
            type="text"
            placeholder="Name of the schema"
            value={name}
            data-key="name"
            onChange={this.handleInputChange.bind(this)}
            disabled={!isNew}
            required
            data-test="remote-schema-schema-name"
            pattern="^[a-zA-Z0-9-_]*$"
            title="Special characters except '-' or '_' are not allowed"
          />
        </label>
        <hr className="my-md" />
        <div className={subHeading}>
          GraphQL server URL *
          <OverlayTrigger placement="right" overlay={tooltips.graphqlurl}>
            <FaQuestionCircle aria-hidden="true" className="mb-1 ml-xs" />
          </OverlayTrigger>
        </div>
        <div className={'w-80'}>
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
            bsClass={`${inputStyles} w-80`}
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
        <div className={`${subHeading} pt-md`}>
          Headers for the remote GraphQL server
        </div>
        <div>
          <label className="mb-md justify-center mr-sm">
            <input
              onChange={this.toggleForwardHeaders.bind(this)}
              className={`${focusYellowRing} m-0`}
              type="checkbox"
              value="forwardHeaders"
              data-test="forward-remote-schema-headers"
              checked={forwardClientHeaders}
              disabled={isDisabled}
            />
            <span className="ml-md">Forward all headers from client</span>
          </label>
          <OverlayTrigger
            placement="right"
            overlay={tooltips.clientHeaderForward}
          >
            <FaQuestionCircle aria-hidden="true" size={'1em'} />
          </OverlayTrigger>
        </div>
        <div className={`${subHeading} font-normal`}>
          Additional headers:
          <OverlayTrigger
            placement="right"
            overlay={tooltips.additionalHeaders}
          >
            <FaQuestionCircle aria-hidden="true" className="mb-1 ml-xs" />
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
        <hr className="my-md" />
        {getTimeoutSection()}
        <hr className="my-md" />
        <div className={subHeading}>
          Comment
          <OverlayTrigger placement="right" overlay={tooltips.comment}>
            <FaQuestionCircle aria-hidden="true" className="mb-1 ml-xs" />
          </OverlayTrigger>
        </div>
        <label className="pl-0">
          <input
            className={`${inputStyles} w-72`}
            type="text"
            placeholder="Comment"
            value={comment}
            data-key="comment"
            onChange={this.handleInputChange.bind(this)}
            disabled={isDisabled}
            data-test="remote-schema-comment"
          />
        </label>
        <hr className="my-lg" />
        {/* <GraphQLCustomization mode="edit" customization={customization} dispatch={this.props.dispatch} /> */}
        {isNew ? null : (
          <>
            <div className="text-lg font-bold">
              GraphQL Customizations{' '}
              <OverlayTrigger
                placement="right"
                overlay={
                  <Tooltip id="tooltip-cascade">
                    Individual Types and Fields will be editable after saving.
                  </Tooltip>
                }
              >
                <FaQuestionCircle aria-hidden="true" className="mb-1 ml-xs" />
              </OverlayTrigger>
            </div>
            <GraphQLCustomizationEdit
              remoteSchemaName={name}
              graphQLCustomization={customization}
              dispatch={this.props.dispatch}
              onChange={this.handleCustomizationInputChange.bind(this)}
              isDisabled={isDisabled}
            />
          </>
        )}
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
