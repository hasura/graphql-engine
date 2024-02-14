import React from 'react';
import PropTypes from 'prop-types';
import { IconTooltip } from '../../../../new-components/Tooltip';
import DropdownButton from '../../../Common/DropdownButton/DropdownButton';

import {
  inputChange,
  UPDATE_FORWARD_CLIENT_HEADERS,
} from '../Add/addRemoteSchemaReducer';

import CommonHeader from '../../../Common/Layout/ReusableHeader/Header';
import GraphQLCustomizationEdit from './GraphQLCustomization/GraphQLCustomizationEdit';

import { focusYellowRing, inputStyles, subHeading } from '../constants';
import { FaShieldAlt } from 'react-icons/fa';
import { LearnMoreLink } from '../../../../new-components/LearnMoreLink';

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
      customization,
      isEnvVarEnabled,
    } = this.props;

    const { isModify } = this.props.editState;

    const isDisabled = !isModify;
    const urlRequired = !manualUrl && !envName;

    const tooltips = {
      graphqlurl: (
        <IconTooltip
          message="Environment variables and secrets are available using the {{VARIABLE}} tag. Environment variable templating is available for this field. Example: https://{{ENV_VAR}}/endpoint_url"
          side="right"
          icon={<FaShieldAlt className="h-4 text-muted cursor-pointer" />}
        />
      ),
      clientHeaderForward: (
        <IconTooltip
          message="Toggle forwarding headers sent by the client app in the request to your remote GraphQL server"
          side="right"
        />
      ),
      additionalHeaders: (
        <IconTooltip
          message="Custom headers to be sent to the remote GraphQL server"
          side="right"
        />
      ),
      schema: (
        <IconTooltip
          message="Give this GraphQL schema a friendly name."
          side="right"
        />
      ),
      timeoutConf: (
        <IconTooltip
          message="Configure timeout for your remote GraphQL server. Defaults to 60 seconds."
          side="right"
        />
      ),
      comment: (
        <IconTooltip
          message="A statement to help describe the remote schema in brief"
          side="right"
        />
      ),
    };

    const getTimeoutSection = () => {
      return (
        <React.Fragment>
          <div className={`${subHeading} flex items-center`}>
            GraphQL server timeout
            {tooltips.timeoutConf}
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
              name="timeout_sec"
            />
          </label>
        </React.Fragment>
      );
    };

    return (
      <div>
        <div className={`${subHeading} pt-md flex items-center`}>
          Remote Schema name *{tooltips.schema}
        </div>
        <label className="ml-0 pl-0 w-80">
          <input
            className={`${inputStyles} disabled:bg-[#EFEFEF]`}
            type="text"
            placeholder="Name of the schema"
            value={name}
            data-key="name"
            onChange={this.handleInputChange.bind(this)}
            disabled
            required
            data-test="remote-schema-schema-name"
            pattern="^[a-zA-Z0-9-_]*$"
            title="Special characters except '-' or '_' are not allowed"
          />
        </label>
        <hr className="my-md" />
        <div className={`${subHeading} flex items-center`}>
          GraphQL server URL *{tooltips.graphqlurl}
          <LearnMoreLink href="https://hasura.io/docs/latest/api-reference/syntax-defs/#webhookurl" />
        </div>
        <p className="text-sm text-gray-600 mb-sm w-1/2">
          Note: Provide an URL or use an env var to template the handler URL if
          you have different URLs for multiple environments.
        </p>
        <div className={'w-1/2'}>
          {isEnvVarEnabled ? (
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
              bsClass="w-80"
              inputVal={envName || manualUrl}
              disabled={isDisabled}
              id="graphql-server-url"
              inputPlaceHolder={
                (manualUrl !== null &&
                  'https://my-graphql-service.com/graphql') ||
                (envName !== null && 'MY_GRAPHQL_ENDPOINT')
              }
              testId="remote-schema-graphql-url"
            />
          ) : (
            <input
              type="text"
              name="handler"
              onChange={this.handleInputChange.bind(this)}
              required={urlRequired}
              className={`w-3/4 ${inputStyles}`}
              data-key={'manualUrl'}
              value={manualUrl}
              disabled={isDisabled}
              id="graphql-server-url"
              placeholder="https://my-graphql-service.com/graphql or {{MY_WEBHOOK_URL}}/graphql"
              data-test="remote-schema-graphql-url"
            />
          )}
        </div>
        <br />
        <div className={`${subHeading} pt-md`}>
          Headers for the remote GraphQL server
        </div>
        <div className={`${subHeading} flex items-center mb-md`}>
          <label className="flex justify-center mr-sm">
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
            {tooltips.clientHeaderForward}
          </label>
        </div>
        <div className={`${subHeading} font-normal flex items-center`}>
          Additional headers:
          {tooltips.additionalHeaders}
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
        <div className={`${subHeading} flex items-center`}>
          Comment
          {tooltips.comment}
        </div>
        <label className="pl-0">
          <input
            className={`${inputStyles} w-72`}
            type="text"
            placeholder="Comment"
            value={comment}
            data-key="comment"
            name="comment"
            onChange={this.handleInputChange.bind(this)}
            disabled={isDisabled}
            data-test="remote-schema-comment"
          />
        </label>
        <hr className="my-lg" />
        <div className="text-lg font-bold flex items-center">
          GraphQL Customizations{' '}
          <IconTooltip
            message="Individual Types and Fields will be editable after saving."
            side="right"
          />
        </div>
        <GraphQLCustomizationEdit
          remoteSchemaName={name}
          graphQLCustomization={customization}
          dispatch={this.props.dispatch}
          onChange={this.handleCustomizationInputChange.bind(this)}
          isDisabled={isDisabled}
        />
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
