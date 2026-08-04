import React, { Component } from 'react';
import {
  FaCheck,
  FaEye,
  FaQuestionCircle,
  FaSpinner,
  FaTimes,
  FaUserSecret,
} from 'react-icons/fa';
import PropTypes from 'prop-types';

import { Button } from '../../../../new-components/Button';
import TextAreaWithCopy from '../../../Common/TextAreaWithCopy/TextAreaWithCopy';
import Modal from '../../../Common/Modal/Modal';
import Tooltip from '../../../Common/Tooltip/Tooltip';

import {
  changeRequestHeader,
  removeRequestHeader,
  focusHeaderTextbox,
  unfocusTypingHeader,
  verifyJWTToken,
  setHeadersBulk,
  switchGraphiQLMode,
} from '../Actions';

import GraphiQLWrapper from '../GraphiQLWrapper/GraphiQLWrapper';
import Toggle from '../../../Common/Toggle/Toggle';

import CollapsibleToggle from '../../../Common/CollapsibleToggle/CollapsibleToggle';

import {
  getEndPointSectionIsOpen,
  setEndPointSectionIsOpen,
  getHeadersSectionIsOpen,
  setHeadersSectionIsOpen,
  persistGraphiQLHeaders,
  getPersistedGraphiQLHeaders,
  parseAuthHeader,
  getDefaultGraphiqlHeaders,
  getAdminSecret,
  getPersistedAdminSecretHeaderWasAdded,
  persistAdminSecretHeaderWasAdded,
  removePersistedAdminSecretHeaderWasAdded,
  persistGraphiQLMode,
} from './utils';
import { getGraphQLEndpoint } from '../utils';

import {
  getLSItem,
  removeLSItem,
  LS_KEYS,
} from '../../../../utils/localStorage';
import {
  ADMIN_SECRET_HEADER_KEY,
  HASURA_CLIENT_NAME,
  HASURA_COLLABORATOR_TOKEN,
} from '../../../../constants';

/* When the page is loaded for the first time, hydrate the header state from the localStorage
 * Keep syncing the localStorage state when user modifies.
 * */

const ActionIcon = ({ message, dataHeaderID }) => (
  <Tooltip placement="left" message={message}>
    <FaQuestionCircle
      className="cursor-pointer mr-sm"
      data-header-id={dataHeaderID}
      aria-hidden="true"
    />
  </Tooltip>
);

class ApiRequest extends Component {
  constructor(props) {
    super(props);

    this.state = {
      endpointSectionIsOpen: getEndPointSectionIsOpen(),
      headersSectionIsOpen: getHeadersSectionIsOpen(),
      adminSecretVisible: false,
      isAnalyzingToken: false,
      analyzingHeaderRow: -1,
      tokenInfo: {
        header: {},
        payload: {},
        error: null,
        serverResp: {},
      },
      jwt: null,
    };

    if (this.props.numberOfTables !== 0) {
      const graphqlQueryInLS = getLSItem(LS_KEYS.graphiqlQuery);
      if (graphqlQueryInLS && graphqlQueryInLS.indexOf('do not have') !== -1) {
        removeLSItem(LS_KEYS.graphiqlQuery);
      }
    }

    this.analyzeBearerToken = this.analyzeBearerToken.bind(this);
    this.onAnalyzeBearerClose = this.onAnalyzeBearerClose.bind(this);

    // Dynamically load jsonwebtoken library to prevent storybook stories crash
    if (!global.window.preventJsonWebTokenLoad) {
      import('jsonwebtoken').then(jwt => {
        this.setState({ ...this.state, jwt });
      });
    }
  }

  componentDidMount() {
    const handleHeaderInit = () => {
      const graphiqlHeaders =
        getPersistedGraphiQLHeaders() || getDefaultGraphiqlHeaders();

      // if admin secret is set and admin secret header was ever added to headers, add admin secret header if not already present
      const adminSecret = getAdminSecret();
      const adminSecretHeaderWasAdded = getPersistedAdminSecretHeaderWasAdded();
      if (adminSecret && !adminSecretHeaderWasAdded) {
        const headerKeys = graphiqlHeaders.map(h => h.key);

        // add admin secret header if not present
        if (!headerKeys.includes(ADMIN_SECRET_HEADER_KEY)) {
          graphiqlHeaders.push({
            key: ADMIN_SECRET_HEADER_KEY,
            value: adminSecret,
            isActive: true,
            isNewHeader: false,
            isDisabled: true,
          });
        }

        // set in local storage that admin secret header has been automatically added
        persistAdminSecretHeaderWasAdded();
      }

      // if admin secret is not set and admin secret header was ever added to headers, remove admin secret header if present
      if (!adminSecret && adminSecretHeaderWasAdded) {
        const headerKeys = graphiqlHeaders.map(h => h.key);

        // remove admin secret header if present
        const adminSecretHeaderIndex = headerKeys.indexOf(
          ADMIN_SECRET_HEADER_KEY
        );
        if (adminSecretHeaderIndex >= 0) {
          graphiqlHeaders.splice(adminSecretHeaderIndex, 1);
        }

        // remove from local storage that admin secret header has been automatically added
        removePersistedAdminSecretHeaderWasAdded();
      }

      // add an empty placeholder header
      graphiqlHeaders.push({
        key: '',
        value: '',
        isActive: true,
        isNewHeader: true,
        isDisabled: false,
      });

      // persist headers to local storage
      persistGraphiQLHeaders(graphiqlHeaders);

      // set headers in redux
      this.props.dispatch(setHeadersBulk(graphiqlHeaders));
    };

    handleHeaderInit();
  }

  onAnalyzeBearerClose() {
    this.setState({
      isAnalyzingToken: false,
      analyzingHeaderRow: -1,
    });
  }

  analyzeBearerToken(token, dataHeaderIndex) {
    return () => {
      const { dispatch } = this.props;

      const analyzingHeaderRow = parseInt(dataHeaderIndex, 10);

      this.setState({
        isAnalyzingToken: true,
        analyzingHeaderRow,
        tokenInfo: {
          ...this.state.tokenInfo,
          serverResp: {},
          error: null,
        },
      });

      const decodeAndSetState = serverResp => {
        const decoded = this.state.jwt.decode(token, { complete: true });

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
    };
  }

  render() {
    const { mode, dispatch, loading } = this.props;
    const { isAnalyzingToken, tokenInfo, analyzingHeaderRow } = this.state;

    const { is_jwt_set: isJWTSet = false } = this.props.serverConfig;

    const { error: tokenAnalyzeError, serverResp: tokenAnalyzeResp } =
      tokenInfo;

    const getGraphQLEndpointBar = () => {
      const { endpointSectionIsOpen } = this.state;

      const toggleHandler = () => {
        const newIsOpen = !endpointSectionIsOpen;

        setEndPointSectionIsOpen(newIsOpen);

        this.setState({ endpointSectionIsOpen: newIsOpen });
      };

      const toggleGraphiqlMode = () => {
        if (loading) return;
        const newMode = mode === 'relay' ? 'graphql' : 'relay';
        persistGraphiQLMode(newMode);
        dispatch(switchGraphiQLMode(newMode));
      };

      return (
        <CollapsibleToggle
          title={'GraphQL Endpoint'}
          isOpen={endpointSectionIsOpen}
          toggleHandler={toggleHandler}
          useDefaultTitleStyle
        >
          <div className="bg-[#f8fafb] pt-sm w-full top-0 z-20">
            <div className={'flex mb-md'}>
              <div className="flex items-center w-full">
                <Button type="button">{this.props.method}</Button>
                <input
                  value={getGraphQLEndpoint(mode)}
                  type="text"
                  readOnly
                  className="flex-1 min-w-0 block w-full px-3 py-2 h-input rounded-r border-gray-300"
                />
              </div>

              <div
                className="flex items-center ml-md cursor-pointer"
                onClick={toggleGraphiqlMode}
              >
                <Toggle
                  checked={mode === 'relay'}
                  className="flex mr-md"
                  readOnly
                  disabled={loading}
                  icons={false}
                />
                <span className="whitespace-nowrap">Relay API</span>
                <Tooltip
                  id="relay-mode-toggle"
                  placement="left"
                  message={
                    'Toggle to point this GraphiQL to a relay-compliant GraphQL API served at /v1beta1/relay'
                  }
                />
              </div>
            </div>
          </div>
        </CollapsibleToggle>
      );
    };

    const getHeaderTable = () => {
      const { headersSectionIsOpen, adminSecretVisible } = this.state;
      const getHeaderRows = () => {
        const headers = this.props.headers;

        const handleFocus = () => {
          this.props.dispatch(focusHeaderTextbox());
        };

        const handleBlur = () => {
          this.props.dispatch(unfocusTypingHeader());
        };

        const onDeleteHeaderClicked = e => {
          const index = parseInt(e.target.getAttribute('data-header-id'), 10);
          this.props
            .dispatch(removeRequestHeader(index))
            .then(r => persistGraphiQLHeaders(r));
        };

        const onHeaderValueChanged = e => {
          const index = parseInt(e.target.getAttribute('data-header-id'), 10);
          const key = e.target.getAttribute('data-element-name');
          const newValue = e.target.value;
          this.props
            .dispatch(changeRequestHeader(index, key, newValue, false))
            .then(r => persistGraphiQLHeaders(r));
        };

        const onShowAdminSecretClicked = () => {
          this.setState({ adminSecretVisible: !this.state.adminSecretVisible });
        };

        return headers.map((header, i) => {
          const isAdminSecret =
            header.key.toLowerCase() === ADMIN_SECRET_HEADER_KEY;

          const consoleId = window.__env.consoleId;

          const isClientName =
            header.key.toLowerCase() === HASURA_CLIENT_NAME && consoleId;

          const isCollaboratorToken =
            header.key.toLowerCase() === HASURA_COLLABORATOR_TOKEN && consoleId;

          const getHeaderActiveCheckBox = () => {
            let headerActiveCheckbox = null;

            if (!header.isNewHeader) {
              headerActiveCheckbox = (
                <td className="text-center">
                  <input
                    type="checkbox"
                    name="sponsored"
                    className="legacy-input-fix mt-xs"
                    id={i + 1}
                    checked={header.isActive}
                    data-header-id={i}
                    onChange={onHeaderValueChanged}
                    data-element-name="isActive"
                  />
                </td>
              );
            } else {
              headerActiveCheckbox = (
                <td className="border-t border-gray-200" />
              );
            }

            return headerActiveCheckbox;
          };

          const getColSpan = () => {
            return header.isNewHeader ? '2' : '1';
          };

          const getHeaderKey = () => {
            let className = '';
            if (header.isNewHeader) {
              className = 'border-r border-t border-gray-200';
            } else {
              className = 'border-r border-gray-200';
            }

            return (
              <td className={className}>
                <input
                  className="w-full border-0 outline-none focus:ring-0 focus:outline-none"
                  value={header.key || ''}
                  disabled={header.isDisabled === true}
                  data-header-id={i}
                  placeholder="Enter Key"
                  name="key"
                  data-element-name="key"
                  onChange={onHeaderValueChanged}
                  onFocus={handleFocus}
                  onBlur={handleBlur}
                  type="text"
                  data-test={`header-key-${i}`}
                  autoComplete="off"
                />
              </td>
            );
          };

          const getHeaderValue = () => {
            let className = '';
            if (header.isNewHeader) {
              className = 'border-t border-gray-200';
            }

            let type = 'text';
            if (isAdminSecret && !adminSecretVisible) {
              type = 'password';
            }

            return (
              <td colSpan={getColSpan()} className={className}>
                <input
                  className="w-full border-0 focus:ring-0 focus:outline-none"
                  value={header.value || ''}
                  disabled={header.isDisabled === true}
                  data-header-id={i}
                  placeholder="Enter Value"
                  name="value"
                  data-element-name="value"
                  onChange={onHeaderValueChanged}
                  onFocus={handleFocus}
                  onBlur={handleBlur}
                  data-test={`header-value-${i}`}
                  type={type}
                  autoComplete="off"
                />
              </td>
            );
          };

          const getHeaderAdminVal = () => {
            let headerAdminVal = null;

            if (isAdminSecret) {
              headerAdminVal = (
                <Tooltip
                  id="admin-secret-show"
                  placement="left"
                  message="Show admin secret"
                >
                  <FaEye
                    className="cursor-pointer mr-sm"
                    data-header-id={i}
                    aria-hidden="true"
                    onClick={onShowAdminSecretClicked}
                  />
                </Tooltip>
              );
            }

            return headerAdminVal;
          };

          const getJWTInspectorIcon = () => {
            // Check whether key is Authorization and value starts with Bearer
            const { isAuthHeader, token } = parseAuthHeader(header);

            let inspectorIcon = null;

            const getAnalyzeIcon = () => {
              let analyzeIcon;

              if (isAnalyzingToken && analyzingHeaderRow === i) {
                analyzeIcon = (
                  <FaSpinner className="cursor-pointer mr-sm animate-spin" />
                );
              } else {
                analyzeIcon = (
                  <FaUserSecret
                    className="cursor-pointer mr-sm"
                    data-header-index={i}
                    onClick={this.analyzeBearerToken(token, i)}
                  />
                );
              }

              return analyzeIcon;
            };

            if (isAuthHeader && isJWTSet) {
              inspectorIcon = (
                <Tooltip
                  id="tooltip-inspect-jwt"
                  message="Decode JWT"
                  placement="left"
                >
                  {getAnalyzeIcon()}
                </Tooltip>
              );
            }

            return inspectorIcon;
          };

          const getHeaderRemoveBtn = () => {
            return (
              <FaTimes
                className="cursor-pointer mr-md"
                data-header-id={i}
                aria-hidden="true"
                onClick={onDeleteHeaderClicked}
              />
            );
          };

          const getHeaderActions = () => {
            let headerActions = null;

            if (!header.isNewHeader) {
              headerActions = (
                <td className="text-right">
                  <div className="flex justify-end items-center">
                    {getHeaderAdminVal()}
                    {getJWTInspectorIcon()}
                    {isClientName && (
                      <ActionIcon
                        message="Hasura client name is a header that indicates where the request is being made from. This is used by GraphQL Engine for providing detailed metrics."
                        dataHeaderID={i}
                      />
                    )}
                    {isCollaboratorToken && (
                      <ActionIcon
                        message="Hasura collaborator token is an admin-secret alternative when you login using Hasura. This is used by GraphQL Engine to authorise your requests."
                        dataHeaderID={i}
                      />
                    )}
                    {getHeaderRemoveBtn()}
                  </div>
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
          <div className="mt-md overflow-x-auto border border-gray-300 rounded">
            <table className="min-w-full divide-y divide-gray-200">
              <thead>
                <tr className="bg-gray-50">
                  <th
                    className={`w-16 px-sm py-xs max-w-xs text-left text-sm font-semibold bg-gray-50 text-gray-600 uppercase tracking-wider align-center`}
                  >
                    Enable
                  </th>
                  <th className="px-sm py-xs max-w-xs text-left text-sm font-semibold bg-gray-50 text-gray-600 uppercase tracking-wider">
                    Key
                  </th>
                  <th className="px-sm py-xs max-w-xs text-left text-sm font-semibold bg-gray-50 text-gray-600 uppercase tracking-wider">
                    Value
                  </th>
                  <th className="w-16 px-sm py-xs max-w-xs text-left text-sm font-semibold bg-gray-50 text-gray-600 uppercase tracking-wider" />
                </tr>
              </thead>
              <tbody className="bg-white">{getHeaderRows()}</tbody>
            </table>
          </div>
        </CollapsibleToggle>
      );
    };

    const getRequestBody = () => {
      switch (this.props.bodyType) {
        case 'graphql':
          return (
            <div
              className={
                'h-[calc(100vh-450px)] min-h-[500px] mt-[20px] mb-[50px] resize-y overflow-auto'
              }
            >
              <GraphiQLWrapper
                mode={mode}
                data={this.props}
                numberOfTables={this.props.numberOfTables}
                dispatch={this.props.dispatch}
                headerFocus={this.props.headerFocus}
                urlParams={this.props.urlParams}
                response={this.props.explorerData.response}
              />
            </div>
          );
        default:
          return '';
      }
    };

    const getAnalyzeTokenModal = () => {
      const getAnalyzeBearerBody = () => {
        const {
          claims_namespace: claimNameSpace = 'https://hasura.io/jwt/claims',
          claims_format: claimFormat = 'json',
        } =
          this.props.serverConfig && this.props.serverConfig.jwt
            ? this.props.serverConfig.jwt
            : {};

        let tokenVerified = false;
        let JWTError = '';
        if (tokenAnalyzeResp && 'errors' in tokenAnalyzeResp) {
          try {
            JWTError =
              tokenAnalyzeResp.errors.length > 0
                ? tokenAnalyzeResp.errors[0].message
                : null;
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
                <Tooltip
                  id="tooltip-jwt-validity-status"
                  message="Valid JWT token"
                >
                  <span className="text-[#28a745]">
                    <FaCheck />
                  </span>
                </Tooltip>
              );
            case !tokenVerified && JWTError.length > 0:
              return (
                <span className="text-[#dc3545]">
                  <FaTimes />
                </span>
              );
            default:
              return null;
          }
        };

        const getJWTFailMessage = () => {
          if (!tokenVerified && JWTError.length > 0) {
            return (
              <div className={'bg-[#e53935] py-sm px-sm text-white rounded-md'}>
                {JWTError}
              </div>
            );
          }
          return null;
        };

        const getHasuraClaims = () => {
          const payload = tokenInfo.payload;
          if (!payload) {
            return null;
          }
          const isValidPayload = Object.keys(payload).length;
          const payloadHasValidNamespace = claimNameSpace in payload;
          const isSupportedFormat =
            ['json', 'stringified_json'].indexOf(claimFormat) !== -1;

          if (
            !isValidPayload ||
            !payloadHasValidNamespace ||
            !isSupportedFormat
          ) {
            return null;
          }

          let claimData = '';

          const generateValidNameSpaceData = claimD => {
            return JSON.stringify(claimD, null, 2);
          };

          try {
            claimData =
              claimFormat === 'stringified_json'
                ? generateValidNameSpaceData(
                    JSON.parse(payload[claimNameSpace])
                  )
                : generateValidNameSpaceData(payload[claimNameSpace]);
          } catch (e) {
            console.error(e);
            return null;
          }

          return [
            <br key="hasura_claim_element_break" />,
            <span
              key="hasura_claim_label"
              className={
                'uppercase m-0 text-black pb-sm border-b border-[#9b9b9b80]'
              }
            >
              Hasura Claims:
              <span>hasura headers</span>
            </span>,
            <TextAreaWithCopy
              key="hasura_claim_value"
              copyText={claimData}
              textLanguage={'json'}
              id="claimNameSpaceCopy"
              containerId="claimNameSpaceCopyBlock"
            />,
            <br key="hasura_claim_element_break_after" />,
          ];
        };

        let analyzeBearerBody;

        if (tokenAnalyzeError) {
          analyzeBearerBody = <span>{tokenAnalyzeError}</span>;
        } else {
          analyzeBearerBody = (
            <div>
              <span
                className={
                  'uppercase m-0 text-black pb-sm border-b border-[#9b9b9b80]'
                }
              >
                Token Validity:
                <span className="text-md">
                  {generateJWTVerificationStatus()}
                </span>
              </span>
              {getJWTFailMessage() || <br />}
              {getHasuraClaims() || <br />}
              <span
                className={
                  'uppercase m-0 text-black pb-sm border-b border-[#9b9b9b80]'
                }
              >
                Header:
                <span>Algorithm & Token Type</span>
              </span>
              <TextAreaWithCopy
                copyText={JSON.stringify(tokenInfo.header, null, 2)}
                textLanguage={'json'}
                id="headerCopy"
                containerId="headerCopyBlock"
              />
              <br />
              <span
                className={
                  'uppercase m-0 text-black pb-sm border-b border-[#9b9b9b80]'
                }
              >
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
        }

        return analyzeBearerBody;
      };

      return (
        <Modal
          show={
            isAnalyzingToken &&
            tokenAnalyzeResp &&
            Object.keys(tokenAnalyzeResp).length > 0
          }
          onClose={this.onAnalyzeBearerClose}
          customClass={'w-[768]'}
          title={tokenAnalyzeError ? 'Error decoding JWT' : 'Decoded JWT'}
        >
          {getAnalyzeBearerBody()}
        </Modal>
      );
    };

    return (
      <div className={`h-full flex flex-col pt-md`}>
        {getGraphQLEndpointBar()}
        {getHeaderTable()}
        {getRequestBody()}
        {getAnalyzeTokenModal()}
      </div>
    );
  }
}

ApiRequest.propTypes = {
  method: PropTypes.string.isRequired,
  url: PropTypes.string.isRequired,
  headers: PropTypes.array,
  dataHeaders: PropTypes.array,
  params: PropTypes.string,
  dispatch: PropTypes.func.isRequired,
  explorerData: PropTypes.object.isRequired,
  credentials: PropTypes.object.isRequired,
  bodyType: PropTypes.string.isRequired,
  route: PropTypes.object,
  numberOfTables: PropTypes.number.isRequired,
  headerFocus: PropTypes.bool.isRequired,
  urlParams: PropTypes.object.isRequired,
  consoleUrl: PropTypes.string.isRequired,
};

export default ApiRequest;
