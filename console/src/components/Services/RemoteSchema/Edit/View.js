import React from 'react';
import CommonTabLayout from '../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import tabInfo from './tabInfo';
import { push } from 'react-router-redux';
import {
  fetchRemoteSchema,
  RESET,
  getHeaderEvents,
} from '../Add/addRemoteSchemaReducer';
import { VIEW_REMOTE_SCHEMA } from '../Actions';
import ReloadRemoteSchema from '../../Settings/MetadataOptions/ReloadRemoteSchema';
import { appPrefix } from '../constants';
import globals from '../../../../Globals';
import styles from '../RemoteSchema.scss';
import ToolTip from '../../../Common/Tooltip/Tooltip';

const prefixUrl = globals.urlPrefix + appPrefix;

const RSHeadersDisplay = ({ data }) =>
  data.length > 0 ? (
    <tr>
      <td>Headers</td>
      <td>
        {data &&
          data
            .filter(header => !!header.name)
            .map((header, index) => [
              <tr key={header}>
                <td>
                  {header.name}:{' '}
                  {header.type === 'static'
                    ? header.value
                    : '<' + header.value + '>'}
                </td>
              </tr>,
              index !== data.length - 1 ? <hr /> : null,
            ])}
      </td>
    </tr>
  ) : null;

const RSReloadSchema = ({ readOnlyMode, remoteSchemaName, ...props }) =>
  !readOnlyMode && remoteSchemaName ? (
    <div className={`${styles.commonBtn} ${styles.detailsRefreshButton}`}>
      <span>
        <ReloadRemoteSchema {...props} remoteSchemaName={remoteSchemaName} />
      </span>
      <span>
        <ToolTip
          icon="fa-question-circle"
          placement="right"
          message="If your remote schema has changed, you need to refresh the GraphQL Engine metadata to query the modified schema"
        />
      </span>
    </div>
  ) : null;

class ViewStitchedSchema extends React.Component {
  componentDidMount() {
    const { remoteSchemaName } = this.props.params;
    if (!remoteSchemaName) {
      this.props.dispatch(push(prefixUrl));
    }
    Promise.all([
      this.props.dispatch(fetchRemoteSchema(remoteSchemaName)),
      this.props.dispatch({ type: VIEW_REMOTE_SCHEMA, data: remoteSchemaName }),
    ]);
  }

  componentDidUpdate(prevProps) {
    if (
      prevProps.params.remoteSchemaName !== this.props.params.remoteSchemaName
    ) {
      Promise.all([
        this.props.dispatch(
          fetchRemoteSchema(this.props.params.remoteSchemaName)
        ),
        this.props.dispatch({
          type: VIEW_REMOTE_SCHEMA,
          data: this.props.params.remoteSchemaName,
        }),
      ]);
    }
  }

  componentWillUnmount() {
    Promise.all([
      this.props.dispatch({ type: RESET }),
      this.props.dispatch({
        type: getHeaderEvents.UPDATE_HEADERS,
        data: [
          {
            name: '',
            type: 'static',
            value: '',
          },
        ],
      }),
      this.props.dispatch({ type: VIEW_REMOTE_SCHEMA, data: '' }),
    ]);
  }

  render() {
    const { remoteSchemaName } = this.props.params;
    const { manualUrl, envName, headers, readOnlyMode } = this.props;

    const filterHeaders = headers.filter(h => !!h.name);

    const breadCrumbs = [
      {
        title: 'Remote schemas',
        url: appPrefix,
      },
      {
        title: 'Manage',
        url: `${appPrefix}/manage`,
      },
    ];

    if (remoteSchemaName) {
      breadCrumbs.push({
        title: remoteSchemaName.trim(),
        url: `${appPrefix}/manage/${remoteSchemaName.trim()}/details`,
      });
      breadCrumbs.push({
        title: 'details',
        url: '',
      });
    }

    let tabInfoCopy = tabInfo;

    if (readOnlyMode) {
      const { modify, ...rest } = tabInfoCopy;
      tabInfoCopy = rest;
    }

    return (
      <div
        className={styles.view_stitch_schema_wrapper + ' ' + styles.addWrapper}
      >
        <CommonTabLayout
          appPrefix={appPrefix}
          currentTab="details"
          heading={remoteSchemaName}
          tabsInfo={tabInfoCopy}
          breadCrumbs={breadCrumbs}
          baseUrl={`${appPrefix}/manage/${remoteSchemaName}`}
        />
        <br />
        <div>
          <div className={styles.detailsSection}>
            <table className="table table-striped table-bordered">
              <thead />
              <tbody>
                <tr>
                  <td>GraphQL Server URL</td>
                  <td>{manualUrl || `<${envName}>`}</td>
                </tr>
                <RSHeadersDisplay data={filterHeaders} />
              </tbody>
            </table>
          </div>
          <RSReloadSchema
            readOnlyMode={readOnlyMode}
            remoteSchemaName={remoteSchemaName}
            {...this.props}
          />
        </div>
        <br />
        <br />
      </div>
    );
  }
}

const mapStateToProps = state => {
  return {
    ...state.remoteSchemas.addData,
    ...state.remoteSchemas.headerData,
    allRemoteSchemas: state.remoteSchemas.listData.remoteSchemas,
    dataHeaders: { ...state.tables.dataHeaders },
    readOnlyMode: state.main.readOnlyMode,
  };
};

export default connect => connect(mapStateToProps)(ViewStitchedSchema);
