import React from 'react';
import PropTypes from 'prop-types';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';
import DropdownButton from './DropdownButton';

import { inputChange } from '../Add/addResolverReducer';

import CommonHeader from '../../Layout/ReusableHeader/Header';

const graphqlurl = (
  <Tooltip id="tooltip-cascade">
    Remote GraphQL server’s URL. E.g. https://my-domai/v1alpha1/graphql
  </Tooltip>
);
const header = (
  <Tooltip id="tooltip-cascade">
    Configure headers for requests to remote GraphQL server. All incoming
    request headers will be forwarded
  </Tooltip>
);
const schema = (
  <Tooltip id="tooltip-cascade">
    Give this GraphQL schema a friendly name.
  </Tooltip>
);

class Common extends React.Component {
  getPlaceHolderText(valType) {
    if (valType === 'static') {
      return 'ABCDEF';
    }
    return 'GRAPHQL_ACCESS_KEY';
  }
  handleInputChange(e) {
    const fieldName = e.target.getAttribute('data-key');
    this.props.dispatch(inputChange(fieldName, e.target.value));
  }
  toggleUrlParam(e) {
    const field = e.target.getAttribute('value');
    this.props.dispatch(inputChange(field, ''));
  }
  render() {
    const styles = require('../Styles.scss');
    const { name, manualUrl, envName } = this.props;
    const { isModify, id } = this.props.editState;
    const isDisabled = id >= 0 && !isModify;
    const urlRequired = !manualUrl && !envName;
    return (
      <div className={styles.CommonWrapper}>
        <div className={styles.subheading_text + ' ' + styles.addPaddTop}>
          Schema name *
          <OverlayTrigger placement="right" overlay={schema}>
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
          />
        </label>
        <hr />
        <div className={styles.subheading_text}>
          GraphQL server URL *
          <OverlayTrigger placement="right" overlay={graphqlurl}>
            <i className="fa fa-question-circle" aria-hidden="true" />
          </OverlayTrigger>
        </div>
        <div className={styles.addPaddCommom}>
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
          />
        </div>
        <div className={styles.subheading_text + ' ' + styles.addPaddTop}>
          Header
          <OverlayTrigger placement="right" overlay={header}>
            <i className="fa fa-question-circle" aria-hidden="true" />
          </OverlayTrigger>
        </div>
        <CommonHeader
          eventPrefix="CUSTOM_RESOLVER"
          headers={this.props.headers}
          dispatch={this.props.dispatch}
          typeOptions={[
            { display_text: 'Value', value: 'static' },
            { display_text: 'From env var', value: 'env' },
          ]}
          isDisabled={isDisabled}
          placeHolderText={this.getPlaceHolderText.bind(this)}
          keyInputPlaceholder="Access-Key"
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
  dispatch: PropTypes.func.isRequired,
};

export default Common;
