import React from 'react';
import PropTypes from 'prop-types';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';

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
  handleInputChange(e) {
    const fieldName = e.target.getAttribute('data-field-name');
    this.props.dispatch(inputChange(fieldName, e.target.value));
  }
  toggleCheckBox(e) {
    const fieldName = e.target.getAttribute('data-field-name');
    this.props.dispatch(inputChange(fieldName, ''));
  }
  render() {
    const styles = require('../Styles.scss');
    const { name, manualUrl, envName } = this.props;
    const { isModify, id } = this.props.editState;
    const isDisabled = id >= 0 && !isModify;
    const urlRequired = !manualUrl.length > 0 && !envName.length > 0;
    return (
      <div className={styles.CommonWrapper}>
        <div className={styles.subheading_text}>
          Remote GraphQL server URL *
          <OverlayTrigger placement="right" overlay={graphqlurl}>
            <i className="fa fa-question-circle" aria-hidden="true" />
          </OverlayTrigger>
        </div>
        <div className={styles.addPaddCommom}>
          <label className={styles.radioLabel + ' radio-inline col-md-3'}>
            <input
              className={styles.radioInput}
              type="radio"
              name="optradio"
              data-field-name="manualUrl"
              onChange={this.toggleCheckBox.bind(this)}
              checked={manualUrl && manualUrl.length > 0 ? true : false}
              disabled={isDisabled}
            />
            Enter manually:
          </label>
          <label
            className={
              styles.inputLabel + ' radio-inline ' + styles.padd_left_remove
            }
          >
            <input
              className={'form-control'}
              type="text"
              placeholder="GraphQL server URL"
              value={manualUrl}
              data-field-name="manualUrl"
              onChange={this.handleInputChange.bind(this)}
              disabled={isDisabled}
              required={urlRequired}
            />
          </label>
        </div>
        <div className={styles.addPaddCommom}>
          <label className={styles.radioLabel + ' radio-inline col-md-3'}>
            <input
              className={styles.radioInput}
              type="radio"
              name="optradio"
              data-field-name="envName"
              onChange={this.toggleCheckBox.bind(this)}
              checked={envName && envName.length > 0 ? true : false}
              disabled={isDisabled}
            />
            Pick from environment variable:
          </label>
          <label
            className={
              styles.inputLabel + ' radio-inline ' + styles.padd_left_remove
            }
          >
            <input
              className={'form-control'}
              type="text"
              placeholder="env_variable_name"
              value={envName}
              data-field-name="envName"
              onChange={this.handleInputChange.bind(this)}
              disabled={isDisabled}
              required={urlRequired}
            />
          </label>
        </div>
        <div className={styles.subheading_text + ' ' + styles.addPaddTop}>
          Header *
          <OverlayTrigger placement="right" overlay={header}>
            <i className="fa fa-question-circle" aria-hidden="true" />
          </OverlayTrigger>
        </div>
        <CommonHeader
          eventPrefix="CUSTOM_RESOLVER"
          headers={this.props.headers}
          dispatch={this.props.dispatch}
          typeOptions={[
            { display: 'static value', value: 'static' },
            { display: 'from env variable', value: 'env' },
          ]}
          isDisabled={isDisabled}
        />
        <div className={styles.subheading_text + ' ' + styles.addPaddTop}>
          Schema alias *
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
            placeholder="My-graphql-schema"
            value={name}
            data-field-name="name"
            onChange={this.handleInputChange.bind(this)}
            disabled={isDisabled}
            required
          />
        </label>
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
