import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { push } from 'react-router-redux';

import {
  changeRequestParams,
  addRequestHeader,
  changeRequestHeader,
  removeRequestHeader,
  updateFileObject,
  focusHeaderTextbox,
  unfocusTypingHeader,
} from './Actions';

import GraphiQLWrapper from './GraphiQLWrapper';

const styles = require('./ApiExplorer.scss');

class ApiRequest extends Component {
  constructor(props) {
    super(props);
    this.state = {};
    this.state.accessKeyVisible = false;
    this.state.bodyAllowedMethods = ['POST'];
    this.state.tabIndex = 0;
  }

  onRequestParamsChanged = newValue => {
    this.props.dispatch(changeRequestParams(newValue));
  };

  onHeaderValueChanged(e) {
    const index = parseInt(e.target.getAttribute('data-header-id'), 10);
    const key = e.target.getAttribute('data-element-name');
    const newValue = e.target.value;
    this.props.dispatch(changeRequestHeader(index, key, newValue, false));
  }

  onDeleteHeaderClicked(e) {
    const index = parseInt(e.target.getAttribute('data-header-id'), 10);
    this.props.dispatch(removeRequestHeader(index));
  }

  onNewHeaderKeyChanged(e) {
    this.handleTypingTimeouts();
    this.props.dispatch(addRequestHeader(e.target.value, ''));
  }

  onNewHeaderValueChanged(e) {
    this.handleTypingTimeouts();
    this.props.dispatch(addRequestHeader('', e.target.value));
  }

  onKeyUpAtNewHeaderField(e) {
    if (e.keyCode === 13) {
      this.props.dispatch(
        addRequestHeader(this.state.newHeader.key, this.state.newHeader.value)
      );
    }
  }
  getUrlBar() {
    return (
      <div
        id="stickyHeader"
        className={
          styles.apiPostRequestWrapper +
          ' ' +
          styles.wd100 +
          ' ' +
          styles.stickyHeader
        }
      >
        <div className={'col-xs-12 ' + styles.padd_remove}>
          <div
            className={
              'input-group ' +
              styles.inputGroupWrapper
            }
          >
            <div
              className={
                'input-group-btn ' +
                styles.inputGroupBtn
              }
            >
              <button type="button" className={'btn btn-default'}>
                POST
              </button>
            </div>
            <input
              defaultValue={this.props.url}
              readOnly
              type="text"
              className={
                styles.inputGroupInput +
                ' form-control '
              }
            />
          </div>
        </div>
        <div className={styles.stickySeparator} />
      </div>
    );
  }

  getHeaderTitleView() {
    return (
      <div className={styles.responseWrapper}>
        <div className={'col-xs-12 ' + styles.padd_remove}>
          <div className={styles.responseHeader}>Request Headers</div>
        </div>
      </div>
    );
  }

  getHeaderRows() {
    const rows = this.props.headers.map((header, i) => {
      return (
        <tr key={i}>
          {header.isNewHeader ? null : (
            <td>
              <input
                type="checkbox"
                name="sponsored"
                className={styles.common_checkbox + ' common_checkbox'}
                id={i + 1}
                checked={header.isActive}
                data-header-id={i}
                onChange={this.onHeaderValueChanged.bind(this)}
                data-element-name="isActive"
              />
              <label
                htmlFor={i + 1}
                className={
                  styles.common_checkbox_label + ' common_checkbox_label'
                }
              />
            </td>
          )}
          <td
            colSpan={header.isNewHeader ? '2' : '1'}
            className={
              header.isNewHeader
                ? styles.border_right +
                  ' ' +
                  styles.tableTdLeft +
                  ' ' +
                  styles.borderTop +
                  ' ' +
                  styles.tableEnterKey
                : styles.border_right
            }
          >
            <input
              className={'form-control ' + styles.responseTableInput}
              value={header.key}
              disabled={header.isDisabled === true ? true : false}
              data-header-id={i}
              placeholder="Enter Key"
              data-element-name="key"
              onChange={this.onHeaderValueChanged.bind(this)}
              onFocus={this.handleFocus}
              onBlur={this.handleBlur}
              type="text"
              data-test={`header-key-${i}`}
            />
          </td>
          <td
            colSpan={header.isNewHeader ? '2' : '1'}
            className={
              header.isNewHeader
                ? styles.borderTop +
                  ' ' +
                  styles.tableEnterKey +
                  ' ' +
                  styles.tableLastTd
                : ''
            }
          >
            <input
              className={'form-control ' + styles.responseTableInput}
              value={header.value}
              disabled={header.isDisabled === true ? true : false}
              data-header-id={i}
              placeholder="Enter Value"
              data-element-name="value"
              onChange={this.onHeaderValueChanged.bind(this)}
              onFocus={this.handleFocus}
              onBlur={this.handleBlur}
              data-test={`header-value-${i}`}
              type={
                header.key === 'X-Hasura-Admin-Secret' &&
                !this.state.accessKeyVisible
                  ? 'password'
                  : 'text'
              }
            />
          </td>
          {header.isNewHeader ? null : (
            <td>
              {header.key === 'X-Hasura-Admin-Secret' ? (
                <i
                  className={styles.showAccessKey + ' fa fa-eye'}
                  data-header-id={i}
                  aria-hidden="true"
                  onClick={this.onShowAccessKeyClicked.bind(this)}
                />
              ) : null}
              <i
                className={styles.closeHeader + ' fa fa-times'}
                data-header-id={i}
                aria-hidden="true"
                onClick={this.onDeleteHeaderClicked.bind(this)}
              />
            </td>
          )}
        </tr>
      );
    });
    return rows;
  }

  getHeaderTableView() {
    return (
      <div className={styles.responseTable}>
        <table className={'table ' + styles.tableBorder}>
          <thead>
            <tr>
              <th className={styles.wd4 + ' ' + styles.headerHeading} />
              <th
                className={
                  styles.wd48 +
                  ' ' +
                  styles.border_right +
                  ' ' +
                  styles.headerHeading
                }
              >
                Key
              </th>
              <th className={styles.wd48 + ' ' + styles.headerHeading}>
                Value
              </th>
              <th className={styles.wd4 + ' ' + styles.headerHeading} />
            </tr>
          </thead>
          <tbody>{this.getHeaderRows()}</tbody>
        </table>
      </div>
    );
  }

  getHeaderBody() {
    return (
      <div className={styles.responseHeader + ' ' + styles.marginBottom}>
        Request Body
      </div>
    );
  }
  getValidBody() {
    switch (this.props.bodyType) {
      case 'graphql':
        return (
          <GraphiQLWrapper
            data={this.props}
            dispatch={this.props.dispatch}
            headerFocus={this.props.headerFocus}
            queryParams={this.props.queryParams}
          />
        );
      default:
        return '';
    }
  }

  changeEndpoint() {
    this.props.dispatch(push('/'));
  }

  handleFocus = () => {
    this.props.dispatch(focusHeaderTextbox());
  };

  handleBlur = () => {
    this.props.dispatch(unfocusTypingHeader());
  };

  handleFileChange(e) {
    if (e.target.files.length > 0) {
      this.props.dispatch(updateFileObject(e.target.files[0]));
    }
  }

  render() {
    return (
      <div className={styles.apiRequestWrapper}>
        {this.getUrlBar()}
        <hr />
        {this.getHeaderTitleView()}
        {this.getHeaderTableView()}
        {this.getValidBody()}
      </div>
    );
  }
}

ApiRequest.propTypes = {
  method: PropTypes.string.isRequired,
  url: PropTypes.string.isRequired,
  headers: PropTypes.array,
  params: PropTypes.string,
  dispatch: PropTypes.func.isRequired,
  explorerData: PropTypes.object.isRequired,
  credentials: PropTypes.object.isRequired,
  bodyType: PropTypes.string.isRequired,
  route: PropTypes.object.isRequired,
  numberOfTables: PropTypes.number.isRequired,
  headerFocus: PropTypes.bool.isRequired,
  queryParams: PropTypes.object.isRequired,
};

export default ApiRequest;
