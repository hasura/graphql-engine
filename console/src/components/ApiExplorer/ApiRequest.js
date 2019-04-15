import React, { Component } from 'react';
import PropTypes from 'prop-types';

import {
  // generateApiCodeClicked,
  // changeRequestMethod,
  // changeRequestUrl,
  // changeRequestParams,
  // addRequestHeader,
  // editGeneratedJson,
  // updateFileObject,
  changeRequestHeader,
  removeRequestHeader,
  focusHeaderTextbox,
  unfocusTypingHeader,
} from './Actions';

import globals from '../../Globals';

import GraphiQLWrapper from './GraphiQLWrapper';

import CollapsibleToggle from '../Common/CollapsibleToggle/CollapsibleToggle';

const styles = require('./ApiExplorer.scss');

class ApiRequest extends Component {
  constructor(props) {
    super(props);

    this.state = {
      deletedHeader: false,
      adminSecretVisible: false,
      bodyAllowedMethods: ['POST'],
      tabIndex: 0,
      timer: null,
    };

    if (this.props.numberOfTables !== 0) {
      const graphqlQueryInLS = window.localStorage.getItem('graphiql:query');
      if (graphqlQueryInLS && graphqlQueryInLS.indexOf('do not have') !== -1) {
        window.localStorage.removeItem('graphiql:query');
      }
    }
  }

  /*
  onUrlChanged = e => {
    this.props.dispatch(changeRequestUrl(e.target.value));
  };

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

  onGenerateApiCodeClicked = () => {
    this.props.dispatch(generateApiCodeClicked());
  };

  onRequestParamsChanged = newValue => {
    this.props.dispatch(changeRequestParams(newValue));
  };

  onEditJsonButtonClick = () => {
    this.props.dispatch(editGeneratedJson());
  };

  getHeaderBody() {
    return (
      <div className={styles.responseHeader + ' ' + styles.marginBottom}>
        Request Body
      </div>
    );
  }

  handleFileChange(e) {
    if (e.target.files.length > 0) {
      this.props.dispatch(updateFileObject(e.target.files[0]));
    }
  }
  */
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
              value={this.props.url || ''}
              type="text"
              readOnly
              className={styles.inputGroupInput + ' form-control '}
            />
          </div>
        </div>
        <div className={styles.stickySeparator} />
      </div>
    );
  }

  getHeaderTableView() {
    const getHeaderRows = () => {
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

        // Case when user loads again.
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

        // Case when user deletes a header from console.
        if (
          stored_headers.length > this.props.headers.length &&
          this.state.deletedHeader === true
        ) {
          this.setState({ deletedHeader: false });
        }
      }

      const headers = this.props.headers;
      localStorage.setItem(
        'HASURA_CONSOLE_GRAPHIQL_HEADERS',
        JSON.stringify(headers)
      );

      const handleFocus = () => {
        this.props.dispatch(focusHeaderTextbox());
      };

      const handleBlur = () => {
        this.props.dispatch(unfocusTypingHeader());
      };

      const onDeleteHeaderClicked = e => {
        const index = parseInt(e.target.getAttribute('data-header-id'), 10);
        this.setState({ deletedHeader: true });
        this.props.dispatch(removeRequestHeader(index));
      };

      const onHeaderValueChanged = e => {
        const index = parseInt(e.target.getAttribute('data-header-id'), 10);
        const key = e.target.getAttribute('data-element-name');
        const newValue = e.target.value;
        this.props.dispatch(changeRequestHeader(index, key, newValue, false));
      };

      const onShowAdminSecretClicked = () => {
        this.setState({ adminSecretVisible: !this.state.adminSecretVisible });
      };

      return headers.map((header, i) => {
        const getHeaderAdminVal = () => {
          let headerAdminVal = null;

          if (
            header.key.toLowerCase() === `x-hasura-${globals.adminSecretLabel}`
          ) {
            headerAdminVal = (
              <i
                className={styles.showAdminSecret + ' fa fa-eye'}
                data-header-id={i}
                aria-hidden="true"
                onClick={onShowAdminSecretClicked}
              />
            );
          }

          return headerAdminVal;
        };

        const getHeaderActiveCheckBox = () => {
          let headerActiveCheckbox = null;

          if (!header.isNewHeader) {
            headerActiveCheckbox = (
              <td>
                <input
                  type="checkbox"
                  name="sponsored"
                  className={styles.common_checkbox + ' common_checkbox'}
                  id={i + 1}
                  checked={header.isActive}
                  data-header-id={i}
                  onChange={onHeaderValueChanged}
                  data-element-name="isActive"
                />
                <label
                  htmlFor={i + 1}
                  className={
                    styles.common_checkbox_label + ' common_checkbox_label'
                  }
                />
              </td>
            );
          }

          return headerActiveCheckbox;
        };

        const getHeaderRemoveBtn = () => {
          return (
            <i
              className={styles.closeHeader + ' fa fa-times'}
              data-header-id={i}
              aria-hidden="true"
              onClick={onDeleteHeaderClicked}
            />
          );
        };

        const getColSpan = () => {
          return header.isNewHeader ? '2' : '1';
        };

        const getHeaderKey = () => {
          let className = '';
          if (header.isNewHeader) {
            className =
              styles.border_right +
              ' ' +
              styles.tableTdLeft +
              ' ' +
              styles.borderTop +
              ' ' +
              styles.tableEnterKey;
          } else {
            className = styles.border_right;
          }

          return (
            <td colSpan={getColSpan()} className={className}>
              <input
                className={'form-control ' + styles.responseTableInput}
                value={header.key || ''}
                disabled={header.isDisabled === true}
                data-header-id={i}
                placeholder="Enter Key"
                data-element-name="key"
                onChange={onHeaderValueChanged}
                onFocus={handleFocus}
                onBlur={handleBlur}
                type="text"
                data-test={`header-key-${i}`}
              />
            </td>
          );
        };

        const getHeaderValue = () => {
          let className = '';
          if (header.isNewHeader) {
            className =
              styles.borderTop +
              ' ' +
              styles.tableEnterKey +
              ' ' +
              styles.tableLastTd;
          }

          let type = 'text';
          if (
            header.key.toLowerCase() ===
              `x-hasura-${globals.adminSecretLabel}` &&
            !this.state.adminSecretVisible
          ) {
            type = 'password';
          }

          return (
            <td colSpan={getColSpan()} className={className}>
              <input
                className={'form-control ' + styles.responseTableInput}
                value={header.value || ''}
                disabled={header.isDisabled === true}
                data-header-id={i}
                placeholder="Enter Value"
                data-element-name="value"
                onChange={onHeaderValueChanged}
                onFocus={handleFocus}
                onBlur={handleBlur}
                data-test={`header-value-${i}`}
                type={type}
              />
            </td>
          );
        };

        const getHeaderActions = () => {
          let headerActions = null;

          if (!header.isNewHeader) {
            headerActions = (
              <td>
                {getHeaderAdminVal()}
                {getHeaderRemoveBtn()}
              </td>
            );
          }

          return headerActions;
        };

        return (
          <tr key={i}>
            {getHeaderActiveCheckBox()}
            {getHeaderKey()}
            {getHeaderValue()}
            {getHeaderActions()}
          </tr>
        );
      });
    };

    return (
      <div className={styles.responseTable + ' ' + styles.remove_all_pad}>
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
          <tbody>{getHeaderRows()}</tbody>
        </table>
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

  render() {
    return (
      <div className={styles.apiRequestWrapper}>
        <CollapsibleToggle title={'GraphQL Endpoint'} isOpen defaultTitle>
          {this.getUrlBar()}
        </CollapsibleToggle>
        <div className={styles.headerWrapper}>
          <CollapsibleToggle
            title={'Request Headers'}
            testId="api-explorer-header"
            isOpen
            defaultTitle
          >
            {this.getHeaderTableView()}
          </CollapsibleToggle>
        </div>
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
  route: PropTypes.object,
  numberOfTables: PropTypes.number.isRequired,
  headerFocus: PropTypes.bool.isRequired,
  queryParams: PropTypes.object.isRequired,
};

export default ApiRequest;
