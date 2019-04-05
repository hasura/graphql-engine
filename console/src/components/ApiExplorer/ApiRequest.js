import React, { Component } from 'react';
import PropTypes from 'prop-types';

import jwt from 'jsonwebtoken';

import TextAreaWithCopy from '../Common/TextAreaWithCopy/TextAreaWithCopy';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';
import ModalWrapper from '../Common/ModalWrapper';

import { parseJWTHeader } from './utils';

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
  setInitialHeaderState,
  verifyJWTToken,
} from './Actions';

import globals from '../../Globals';

import GraphiQLWrapper from './GraphiQLWrapper';

import CollapsibleToggle from '../Common/CollapsibleToggle/CollapsibleToggle';

const styles = require('./ApiExplorer.scss');

const inspectJWTTooltip = (
  <Tooltip id="tooltip-inspect-jwt">Decode JWT</Tooltip>
);

const jwtValidityStatus = message => (
  <Tooltip id="tooltip-jwt-validity-status">{message}</Tooltip>
);

/* When the page is loaded for the first time, hydrate the header state from the localStorage
 * Keep syncing the localStorage state when user modifies.
 * */

class ApiRequest extends Component {
  constructor(props) {
    super(props);
    this.defaultTokenVal = {
      header: {},
      payload: {},
      error: null,
      serverResp: {},
    };
    this.state = {
      isAnalyzingBearer: false,
      tokenInfo: {
        ...this.defaultTokenVal,
      },
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
    this.analyzeBearerToken = this.analyzeBearerToken.bind(this);
  }

  componentDidMount() {
    const { headers } = this.props;
    const HEADER_FROM_LS = localStorage.getItem(
      'HASURA_CONSOLE_GRAPHIQL_HEADERS'
    );
    if (HEADER_FROM_LS) {
      try {
        const initialHeader = JSON.parse(HEADER_FROM_LS);
        this.props.dispatch(setInitialHeaderState(initialHeader));
      } catch (e) {
        console.error(e);
        localStorage.setItem(
          'HASURA_CONSOLE_GRAPHIQL_HEADERS',
          JSON.stringify(headers)
        );
        // Ignore
      }
    } else {
      localStorage.setItem(
        'HASURA_CONSOLE_GRAPHIQL_HEADERS',
        JSON.stringify(headers)
      );
    }
  }

  onAnalyzeBearerClose() {
    this.setState({
      isAnalyzingBearer: false,
    });
  }

  /*
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
    this.props
      .dispatch(changeRequestHeader(index, key, newValue, false))
      .then(r => this.setLocalStorageHeader(r));
  }

  onDeleteHeaderClicked(e) {
    const index = parseInt(e.target.getAttribute('data-header-id'), 10);
    // this.setState({ deletedHeader: true });
    this.props
      .dispatch(removeRequestHeader(index))
      .then(r => this.setLocalStorageHeader(r));
  }

  onShowAdminSecretClicked() {
    this.setState({ adminSecretVisible: !this.state.adminSecretVisible });
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

  /*
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
  } */

  onHeaderValueChanged(e) {
    const index = parseInt(e.target.getAttribute('data-header-id'), 10);
    const key = e.target.getAttribute('data-element-name');
    const newValue = e.target.value;
    this.props
      .dispatch(changeRequestHeader(index, key, newValue, false))
      .then(r => this.setLocalStorageHeader(r));
  }

  onDeleteHeaderClicked(e) {
    const index = parseInt(e.target.getAttribute('data-header-id'), 10);
    this.props
      .dispatch(removeRequestHeader(index))
      .then(r => this.setLocalStorageHeader(r));
  }

  onShowAdminSecretClicked() {
    this.setState({ adminSecretVisible: !this.state.adminSecretVisible });
  }

  setLocalStorageHeader = headers => {
    localStorage.setItem(
      'HASURA_CONSOLE_GRAPHIQL_HEADERS',
      JSON.stringify(headers)
    );
  };

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

  getCollapsibleTitle(title) {
    return (
      <div className={styles.responseWrapper}>
        <div className={styles.responseHeader}>{title}</div>
      </div>
    );
  }

  getHeaderRows() {
    /*
     *  - Implementation notes
     *    - header object is an array of the object of the form
     *      - {"key":"content-type","value":"application/json","isActive":true,"isNewHeader":false,"isDisabled":true}
     *      * isNewHeader determines whether a row is an unfilled row so that when user enters, a next new row can be created.
     *      * isActive determines whether that header key is active and ok to be sent in the request header.
     *      * isDisabled determines whether changes for that header key, val is allowed or not
     * */
    const { is_jwt_set: isJWTSet = false } = this.props.serverConfig;

    return this.props.headers.map((header, i) => {
      /*
       * - Check whether key is Authorization and value starts with Bearer
       * */
      const { isJWTHeader: showInspector, matches } = parseJWTHeader(header);

      const inspectorIcon = () => {
        const getAnalyzeIcon = () => {
          const { isAnalyzingBearer } = this.state;
          return isAnalyzingBearer ? (
            <i
              className={styles.showInspectorLoading + ' fa fa-spinner fa-spin'}
            />
          ) : (
            <i
              className={styles.showInspector + ' fa fa-plus-square-o'}
              token={matches[2]}
              onClick={this.analyzeBearerToken}
            />
          );
        };
        return showInspector && isJWTSet ? (
          <OverlayTrigger placement="top" overlay={inspectJWTTooltip}>
            {getAnalyzeIcon()}
          </OverlayTrigger>
        ) : null;
      };

      const getHeaderAdminVal = () => {
        const adminHeader = `x-hasura-${globals.adminSecretLabel}`;
        return header.key.toLowerCase() === adminHeader ? (
          <i
            className={styles.showAdminSecret + ' fa fa-eye'}
            data-header-id={i}
            aria-hidden="true"
            onClick={this.onShowAdminSecretClicked.bind(this)}
          />
        ) : null;
      };

      const getHeaderActiveCheckBox = () => {
        return !header.isNewHeader ? (
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
        ) : null;
      };

      const getHeaderRemoveBtn = () => {
        return (
          <i
            className={styles.closeHeader + ' fa fa-times'}
            data-header-id={i}
            aria-hidden="true"
            onClick={this.onDeleteHeaderClicked.bind(this)}
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
              onChange={this.onHeaderValueChanged.bind(this)}
              onFocus={this.handleFocus}
              onBlur={this.handleBlur}
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
          header.key.toLowerCase() === `x-hasura-${globals.adminSecretLabel}` &&
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
              onChange={this.onHeaderValueChanged.bind(this)}
              onFocus={this.handleFocus}
              onBlur={this.handleBlur}
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
              {inspectorIcon()}
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
  }

  getHeaderTableView() {
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
          <tbody>{this.getHeaderRows()}</tbody>
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

  analyzeBearerToken(e) {
    const { dispatch } = this.props;
    const token = e.target.getAttribute('token');
    this.setState({
      isAnalyzingBearer: true,
      tokenInfo: {
        ...this.state.tokenInfo,
        serverResp: {},
        error: null,
      },
    });
    const decodeAndSetState = serverResp => {
      const decoded = jwt.decode(token, { complete: true });
      if (decoded) {
        this.setState({
          tokenInfo: {
            ...this.state.tokenInfo,
            header: decoded.header,
            payload: decoded.payload,
            error: null,
            serverResp: serverResp,
          },
        });
      } else {
        const message =
          'This JWT seems to be invalid. Please check the token value and try again!';
        this.setState({
          tokenInfo: {
            ...this.state.tokenInfo,
            error: message,
            serverResp: serverResp,
          },
        });
      }
    };
    if (token) {
      dispatch(verifyJWTToken(token))
        .then(data => {
          decodeAndSetState(data);
        })
        .catch(err => {
          decodeAndSetState(err);
        });
    }
  }

  handleFocus = () => {
    this.props.dispatch(focusHeaderTextbox());
  };

  handleBlur = () => {
    this.props.dispatch(unfocusTypingHeader());
  };

  render() {
    const { isAnalyzingBearer, tokenInfo } = this.state;
    const { is_jwt_set: isJWTSet = false } = this.props.serverConfig;
    const { error, serverResp } = tokenInfo;
    const getAnalyzeBearerBody = () => {
      const {
        claims_namespace: claimNameSpace = 'https://hasura.io/jwt/claims',
        claims_format: claimFormat = 'json',
      } = this.props.serverConfig;

      let tokenVerified = false;
      let JWTError = '';
      if (serverResp && 'errors' in serverResp) {
        try {
          JWTError =
            serverResp.errors.length > 0 ? serverResp.errors[0].message : null;
        } catch (e) {
          JWTError = e.toString();
        }
      } else {
        tokenVerified = true;
      }

      const generateJWTVerificationStatus = () => {
        switch (true) {
          case tokenVerified:
            return (
              <OverlayTrigger
                placement="top"
                overlay={jwtValidityStatus('Valid JWT token')}
              >
                <span className={styles.valid_jwt_token}>
                  <i className="fa fa-check" />
                </span>
              </OverlayTrigger>
            );
          case !tokenVerified && JWTError.length > 0:
            return (
              <OverlayTrigger
                placement="top"
                overlay={jwtValidityStatus(JWTError)}
              >
                <span className={styles.invalid_jwt_icon}>
                  <i className="fa fa-times" />
                </span>
              </OverlayTrigger>
            );
          default:
            return null;
        }
      };

      const getHasuraClaims = () => {
        return claimNameSpace &&
          claimFormat === 'json' &&
          tokenInfo.payload &&
          Object.keys(tokenInfo.payload).length > 0
          ? [
            <br key="hasura_claim_element_break" />,
            <span key="hasura_claim_label" className={styles.analyzerLabel}>
                Hasura Claims:
              <span>hasura headers</span>
            </span>,
            <TextAreaWithCopy
              key="hasura_claim_value"
              copyText={JSON.stringify(
                tokenInfo.payload[claimNameSpace],
                null,
                2
              )}
              textLanguage={'json'}
              id="claimNameSpaceCopy"
              containerId="claimNameSpaceCopyBlock"
            />,
          ]
          : null;
      };

      const analyzeBearerBody = error ? (
        <span>{error}</span>
      ) : (
        <div>
          <span className={styles.analyzerLabel}>
            Token Validity:
            <span className={styles.token_validity}>
              {generateJWTVerificationStatus()}
            </span>
          </span>
          <br />
          <span className={styles.analyzerLabel}>
            Header:
            <span>Algorithm & Token Type</span>
          </span>
          <TextAreaWithCopy
            copyText={JSON.stringify(tokenInfo.header, null, 2)}
            textLanguage={'json'}
            id="headerCopy"
            containerId="headerCopyBlock"
          />
          {getHasuraClaims()}
          <br />
          <span className={styles.analyzerLabel}>
            Full Payload:
            <span>Data</span>
          </span>
          <TextAreaWithCopy
            copyText={JSON.stringify(tokenInfo.payload, null, 2)}
            textLanguage={'json'}
            id="payloadCopy"
            containerId="payloadCopyBlock"
          />
        </div>
      );
      return analyzeBearerBody;
    };

    const analyzeBearerHtml = (
      <ModalWrapper
        show={
          isAnalyzingBearer && serverResp && Object.keys(serverResp).length > 0
        }
        onHide={this.onAnalyzeBearerClose.bind(this)}
        dialogClassName={styles.analyzerBearerModal}
        title={error ? 'Error decoding JWT' : 'Decoded JWT'}
      >
        {getAnalyzeBearerBody()}
      </ModalWrapper>
    );
    return (
      <div className={styles.apiRequestWrapper}>
        <CollapsibleToggle
          title={this.getCollapsibleTitle('GraphQL Endpoint')}
          isOpen
        >
          {this.getUrlBar()}
        </CollapsibleToggle>
        <div className={styles.headerWrapper}>
          <CollapsibleToggle
            title={this.getCollapsibleTitle('Request Headers')}
            testId="api-explorer-header"
            isOpen
          >
            {this.getHeaderTableView()}
          </CollapsibleToggle>
        </div>
        {this.getValidBody()}
        {isJWTSet && analyzeBearerHtml}
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
  serverConfig: PropTypes.object,
};

export default ApiRequest;
