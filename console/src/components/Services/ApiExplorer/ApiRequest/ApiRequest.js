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
} from '../Actions';

import {
  getEndPointSectionIsOpen,
  setEndPointSectionIsOpen,
  getHeadersSectionIsOpen,
  setHeadersSectionIsOpen,
  getGraphiQLHeadersFromLocalStorage,
  setGraphiQLHeadersInLocalStorage,
} from './utils';

import globals from '../../../../Globals';

import GraphiQLWrapper from '../GraphiQLWrapper/GraphiQLWrapper';

import CollapsibleToggle from '../../../Common/CollapsibleToggle/CollapsibleToggle';

import styles from '../ApiExplorer.scss';

class ApiRequest extends Component {
  constructor(props) {
    super(props);

    this.state = {
      deletedHeader: false,
      adminSecretVisible: false,
      bodyAllowedMethods: ['POST'],
      tabIndex: 0,
      timer: null,
      endpointSectionIsOpen: getEndPointSectionIsOpen(),
      headersSectionIsOpen: getHeadersSectionIsOpen(),
    };
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
  render() {
    const getGraphQLEndpointBar = () => {
      const { endpointSectionIsOpen } = this.state;

      const toggleHandler = () => {
        const newIsOpen = !endpointSectionIsOpen;

        setEndPointSectionIsOpen(newIsOpen);

        this.setState({ endpointSectionIsOpen: newIsOpen });
      };

      return (
        <CollapsibleToggle
          title={'GraphQL Endpoint'}
          isOpen={endpointSectionIsOpen}
          toggleHandler={toggleHandler}
          useDefaultTitleStyle
        >
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
        </CollapsibleToggle>
      );
    };

    const getHeaderTable = () => {
      const { headersSectionIsOpen } = this.state;

      const getHeaderRows = () => {
        const headers_map = new Map();

        const localStorageHeaders = getGraphiQLHeadersFromLocalStorage();
        if (localStorageHeaders) {
          const stored_headers = JSON.parse(localStorageHeaders);

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

        setGraphiQLHeadersInLocalStorage(JSON.stringify(headers));

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
              header.key.toLowerCase() ===
              `x-hasura-${globals.adminSecretLabel}`
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

      const toggleHandler = () => {
        const newIsOpen = !headersSectionIsOpen;

        setHeadersSectionIsOpen(newIsOpen);

        this.setState({ headersSectionIsOpen: newIsOpen });
      };

      return (
        <CollapsibleToggle
          title={'Request Headers'}
          testId="api-explorer-header"
          isOpen={headersSectionIsOpen}
          toggleHandler={toggleHandler}
          useDefaultTitleStyle
        >
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
        </CollapsibleToggle>
      );
    };

    const getRequestBody = () => {
      switch (this.props.bodyType) {
        case 'graphql':
          return (
            <div className={styles.add_mar_top}>
              <GraphiQLWrapper
                data={this.props}
                numberOfTables={this.props.numberOfTables}
                dispatch={this.props.dispatch}
                headerFocus={this.props.headerFocus}
                urlParams={this.props.urlParams}
              />
            </div>
          );
        default:
          return '';
      }
    };

    return (
      <div className={styles.apiRequestWrapper}>
        {getGraphQLEndpointBar()}
        {getHeaderTable()}
        {getRequestBody()}
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
  urlParams: PropTypes.object.isRequired,
};

export default ApiRequest;
