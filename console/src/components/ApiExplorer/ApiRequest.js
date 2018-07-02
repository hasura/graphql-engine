import React, { Component } from 'react';
import PropTypes from 'prop-types';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';
import globals from 'Globals';

import {
  generateApiCodeClicked,
  changeRequestMethod,
  changeRequestUrl,
  changeRequestParams,
  sendExplorerReq,
  addRequestHeader,
  changeRequestHeader,
  removeRequestHeader,
  updateFileObject,
  editGeneratedJson,
} from './Actions';

// Import query builder

import GraphiQLWrapper from './GraphiQLWrapper';

const adminToken = (
  <Tooltip id="tooltip-admin-token">
    The access key is the static key for querying Data API as Admin
  </Tooltip>
);

const styles = require('./ApiExplorer.scss');

class ApiRequest extends Component {
  constructor(props) {
    super(props);
    this.state = {};
    this.state.bodyAllowedMethods = ['POST'];
    this.state.tabIndex = 0;
  }

  onGenerateApiCodeClicked = () => {
    this.props.dispatch(generateApiCodeClicked());
  };

  onSendButtonClick = () => {
    // check the request type
    this.props.dispatch(sendExplorerReq(this.props.bodyType));
  };

  onUrlChanged = e => {
    this.props.dispatch(changeRequestUrl(e.target.value));
  };

  onRequestParamsChanged = newValue => {
    this.props.dispatch(changeRequestParams(newValue));
  };

  onEditJsonButtonClick = () => {
    this.props.dispatch(editGeneratedJson());
  };

  onHeaderValueChanged(e) {
    const index = parseInt(e.target.getAttribute('data-header-id'), 10);
    const key = e.target.getAttribute('data-element-name');
    const newValue = e.target.value;
    this.props.dispatch(changeRequestHeader(index, key, newValue));
  }

  onDeleteHeaderClicked(e) {
    const index = parseInt(e.target.getAttribute('data-header-id'), 10);
    this.props.dispatch(removeRequestHeader(index));
  }

  onNewHeaderKeyChanged(e) {
    this.props.dispatch(addRequestHeader(e.target.value, ''));
  }

  onNewHeaderValueChanged(e) {
    this.props.dispatch(addRequestHeader('', e.target.value));
  }

  onKeyUpAtNewHeaderField(e) {
    if (e.keyCode === 13) {
      this.props.dispatch(
        addRequestHeader(this.state.newHeader.key, this.state.newHeader.value)
      );
    }
  }

  onAddAdminTokenClicked = () => {
    this.props.dispatch(
      addRequestHeader('X-HASURA-ACCESS-KEY', globals.accessKey)
    );
  };

  getHTTPMethods = () => {
    const httpMethods = ['POST'];
    const scopedThis = this;
    return httpMethods.map(method => {
      return (
        <li
          key={method}
          onClick={() => {
            scopedThis.props.dispatch(changeRequestMethod(method));
          }}
        >
          <a href="#">{method}</a>
        </li>
      );
    });
  };

  getUrlBar() {
    const { explorerData, bodyType } = this.props;

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
              styles.inputGroupWrapper +
              ' ' +
              styles.cursorNotAllowed
            }
          >
            <div className={'input-group-btn ' + styles.inputGroupBtn}>
              <button type="button" className={'btn btn-default'}>
                {this.props.method}
              </button>
            </div>
            <input
              onChange={this.onUrlChanged}
              value={this.props.url}
              type="text"
              className={
                styles.inputGroupInput +
                ' form-control ' +
                styles.cursorNotAllowed
              }
            />
          </div>
        </div>
        {this.props.bodyType !== 'graphql' ? (
          <div className={'col-xs-2 ' + styles.wd16}>
            <div className={styles.sendBtn}>
              {!explorerData.sendingRequest ? (
                <button
                  onClick={() => {
                    this.onSendButtonClick();
                  }}
                >
                  {bodyType === 'download' ? 'Download' : 'Send'}
                </button>
              ) : (
                <button
                  style={{ opacity: 0.4 }}
                  className="btn"
                  disabled={explorerData.sendingRequest}
                >
                  Sending...
                </button>
              )}
            </div>
          </div>
        ) : null}
        {this.props.bodyType !== 'graphql' ? (
          <div className={'col-xs-3 ' + styles.padd_remove + ' ' + styles.wd16}>
            <div
              onClick={this.onGenerateApiCodeClicked}
              className={styles.generateBtn}
            >
              <button className="btn">Generate API code</button>
            </div>
          </div>
        ) : null}
        <div className={styles.stickySeparator} />
      </div>
    );
  }

  getHeaderTitleView() {
    let addAdminToken = null;
    if (globals.accessKey && globals.accessKey !== '') {
      addAdminToken = (
        <span className={styles.addAdminToken}>
          <button
            onClick={this.onAddAdminTokenClicked}
            type="button"
            className={'btn btn-default btn-xs'}
          >
            Add Access Key
          </button>
          <OverlayTrigger placement="right" overlay={adminToken}>
            <i
              className={
                styles.admin_token_align +
                ' ' +
                styles.padd_small_left +
                ' fa fa-info-circle'
              }
              aria-hidden="true"
            />
          </OverlayTrigger>
        </span>
      );
    }
    return (
      <div className={styles.responseWrapper}>
        <div className={'col-xs-12 ' + styles.padd_remove}>
          <div className={styles.responseHeader}>
            Request Headers
            {addAdminToken}
          </div>
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
              data-header-id={i}
              placeholder="Enter Key"
              data-element-name="key"
              onChange={this.onHeaderValueChanged.bind(this)}
              type="text"
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
              data-header-id={i}
              placeholder="Enter Value"
              data-element-name="value"
              onChange={this.onHeaderValueChanged.bind(this)}
              type="text"
            />
          </td>
          {header.isNewHeader ? null : (
            <td>
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
        return <GraphiQLWrapper data={this.props} />;
      default:
        return '';
    }
  }

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
};

export default ApiRequest;
