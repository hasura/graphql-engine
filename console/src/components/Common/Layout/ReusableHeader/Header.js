import React from 'react';
import PropTypes from 'prop-types';

import { generateHeaderSyms } from './HeaderReducer';
import DropdownButton from '../../DropdownButton/DropdownButton';

class Header extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      ...generateHeaderSyms(props.eventPrefix),
    };
  }
  componentWillUnmount() {
    // Reset the header whenever it is unmounted
    this.props.dispatch({
      type: this.state.RESET_HEADER,
    });
  }
  getIndex(e) {
    const indexId = e.target.getAttribute('data-index-id');
    return parseInt(indexId, 10);
  }
  getTitle(val, k) {
    return val.filter(v => v.value === k);
  }
  headerKeyChange(e) {
    const indexId = this.getIndex(e);
    if (indexId < 0) {
      console.error('Unable to handle event');
      return;
    }
    Promise.all([
      this.props.dispatch({
        type: this.state.HEADER_KEY_CHANGE,
        data: {
          name: e.target.value,
          index: indexId,
        },
      }),
    ]);
  }
  checkAndAddNew(e) {
    const indexId = this.getIndex(e);
    if (indexId < 0) {
      console.error('Unable to handle event');
      return;
    }
    if (
      this.props.headers[indexId].name &&
      this.props.headers[indexId].name.length > 0 &&
      indexId === this.props.headers.length - 1
    ) {
      Promise.all([this.props.dispatch({ type: this.state.ADD_NEW_HEADER })]);
    }
  }
  headerValueChange(e) {
    const indexId = this.getIndex(e);
    if (indexId < 0) {
      console.error('Unable to handle event');
      return;
    }
    this.props.dispatch({
      type: this.state.HEADER_VALUE_CHANGE,
      data: {
        value: e.target.value,
        index: indexId,
      },
    });
  }
  headerTypeChange(e) {
    const indexId = this.getIndex(e);
    const typeValue = e.target.getAttribute('value');
    if (indexId < 0) {
      console.error('Unable to handle event');
      return;
    }
    this.props.dispatch({
      type: this.state.HEADER_VALUE_TYPE_CHANGE,
      data: {
        type: typeValue,
        index: indexId,
      },
    });
  }
  deleteHeader(e) {
    const indexId = this.getIndex(e);
    if (indexId < 0) {
      console.error('Unable to handle event');
      return;
    }
    this.props.dispatch({
      type: this.state.DELETE_HEADER,
      data: {
        type: e.target.value,
        index: indexId,
      },
    });
  }

  render() {
    const styles = require('./Header.scss');
    const { isDisabled } = this.props;
    const generateHeaderHtml = this.props.headers.map((h, i) => {
      const getTitle = this.getTitle(this.props.typeOptions, h.type);
      return (
        <div
          className={
            styles.common_header_wrapper +
            ' ' +
            styles.display_flex +
            ' form-group'
          }
          key={i}
        >
          <input
            type="text"
            className={
              styles.input +
              ' form-control ' +
              styles.add_mar_right +
              ' ' +
              styles.defaultWidth
            }
            data-index-id={i}
            value={h.name}
            onChange={this.headerKeyChange.bind(this)}
            onBlur={this.checkAndAddNew.bind(this)}
            placeholder={this.props.keyInputPlaceholder}
            disabled={isDisabled}
            data-test={`remote-schema-header-test${i + 1}-key`}
          />
          <span className={styles.header_colon}>:</span>
          <span className={styles.value_wd}>
            <DropdownButton
              dropdownOptions={this.props.typeOptions}
              title={getTitle.length > 0 ? getTitle[0].display_text : 'Value'}
              dataKey={h.type}
              dataIndex={i}
              onButtonChange={this.headerTypeChange.bind(this)}
              onInputChange={this.headerValueChange.bind(this)}
              inputVal={h.value}
              disabled={isDisabled}
              id={'common-header-' + (i + 1)}
              inputPlaceHolder={this.props.placeHolderText(h.type)}
              testId={`remote-schema-header-test${i + 1}`}
            />
          </span>
          {/*
          <select
            className={
              'form-control ' +
              styles.add_pad_left +
              ' ' +
              styles.add_mar_right +
              ' ' +
              styles.defaultWidth
            }
            value={h.type}
            onChange={this.headerTypeChange.bind(this)}
            data-index-id={i}
            disabled={isDisabled}
          >
            <option disabled value="">
              -- value type --
            </option>
            {this.props.typeOptions.map((o, k) => (
              <option key={k} value={o.value} data-index-id={i}>
                {o.display}
              </option>
            ))}
          </select>
          <input
            type="text"
            className={
              styles.inputDefault +
              ' form-control ' +
              styles.defaultWidth +
              ' ' +
              styles.add_pad_left
            }
            placeholder="value"
            value={h.value}
            onChange={this.headerValueChange.bind(this)}
            data-index-id={i}
            disabled={isDisabled}
          />
          */}
          {i !== this.props.headers.length - 1 && !isDisabled ? (
            <i
              className={styles.fontAwosomeClose + ' fa-lg fa fa-times'}
              onClick={this.deleteHeader.bind(this)}
              data-index-id={i}
            />
          ) : null}
        </div>
      );
    });
    return <div className={this.props.wrapper_class}>{generateHeaderHtml}</div>;
  }
}

Header.propTypes = {
  headers: PropTypes.array,
  isDisabled: PropTypes.bool,
  typeOptions: PropTypes.array,
  placeHolderText: PropTypes.string,
  keyInputPlaceholder: PropTypes.string,
};

export default Header;
