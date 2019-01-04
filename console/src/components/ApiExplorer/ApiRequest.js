import React, { Component } from 'react';
import PropTypes from 'prop-types';

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
  focusHeaderTextbox,
  unfocusTypingHeader,
} from './Actions';

import GraphiQLWrapper from './GraphiQLWrapper';

const styles = require('./ApiExplorer.scss');

class ApiRequest extends Component {
  constructor(props) {
    super(props);
    this.state = {
      deletedHeader: false,
    };
    this.state.accessKeyVisible = false;
    this.state.bodyAllowedMethods = ['POST'];
    this.state.tabIndex = 0;
    this.timer = null;
    if (this.props.numberOfTables !== 0) {
      const graphqlQueryInLS = window.localStorage.getItem('graphiql:query');
      if (graphqlQueryInLS && graphqlQueryInLS.indexOf('do not have') !== -1) {
        window.localStorage.removeItem('graphiql:query');
      }
    }
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
    this.props.dispatch(changeRequestHeader(index, key, newValue, false));
  }

  onDeleteHeaderClicked(e) {
    const index = parseInt(e.target.getAttribute('data-header-id'), 10);
    this.setState({ deletedHeader: true });
    this.props.dispatch(removeRequestHeader(index));
  }

  onShowAccessKeyClicked() {
    this.setState({ accessKeyVisible: !this.state.accessKeyVisible });
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
    return (
      <div className={styles.responseWrapper}>
        <div className={'col-xs-12 ' + styles.padd_remove}>
          <div className={styles.responseHeader}>Request Headers</div>
        </div>
      </div>
    );
  }

  getHeaderRows() {
    let headers;
    const headers_map = new Map();
    if (localStorage.getItem('HASURA_CONSOLE_GRAPHIQL_HEADERS')) {
      const stored_headers = JSON.parse(
        localStorage.getItem('HASURA_CONSOLE_GRAPHIQL_HEADERS')
      );
      for (const s_h of this.props.headers) {
        if (!headers_map.has(s_h.key)) {
          headers_map.set(s_h.key, 1);
        }
      }
      //Case when user loads again.
      if (
        stored_headers.length > this.props.headers.length &&
        this.state.deletedHeader === false
      ) {
        const initHeaderCount = this.props.headers.length - 1;
        const input_row = this.props.headers.pop();
        for (
          let i = initHeaderCount;
          i <= stored_headers.length - initHeaderCount;
          i++
        ) {
          if (!headers_map.has(stored_headers[i].key)) {
            this.props.headers.push(stored_headers[i]);
          }
        }
        this.props.headers.push(input_row);
      }
      //Case when user deletes a header from console.
      if (
        stored_headers.length > this.props.headers.length &&
        this.state.deletedHeader === true
      ) {
        this.setState({ deletedHeader: false });
      }
      headers = this.props.headers;
      localStorage.setItem(
        'HASURA_CONSOLE_GRAPHIQL_HEADERS',
        JSON.stringify(headers)
      );
    } else {
      headers = this.props.headers;
      localStorage.setItem(
        'HASURA_CONSOLE_GRAPHIQL_HEADERS',
        JSON.stringify(headers)
      );
    }
    const rows = headers.map((header, i) => {
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
                header.key.toLowerCase() === 'x-hasura-access-key' &&
                !this.state.accessKeyVisible
                  ? 'password'
                  : 'text'
              }
            />
          </td>
          {header.isNewHeader ? null : (
            <td>
              {header.key.toLowerCase() === 'x-hasura-access-key' ? (
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
            numberOfTables={this.props.numberOfTables}
            dispatch={this.props.dispatch}
            headerFocus={this.props.headerFocus}
            queryParams={this.props.queryParams}
          />
        );
      default:
        return '';
    }
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
