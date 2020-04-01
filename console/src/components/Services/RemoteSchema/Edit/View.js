import React from 'react';
import { push } from 'react-router-redux';

import CommonTabLayout from '../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import tabInfo from './tabInfo';

import {
  fetchRemoteSchema,
  RESET,
  getHeaderEvents,
} from '../Add/addRemoteSchemaReducer';

import { VIEW_REMOTE_SCHEMA } from '../Actions';
import ReloadRemoteSchema from '../../Settings/MetadataOptions/ReloadRemoteSchema';

import { ToolTip } from '../../../UIKit/atoms';

import { appPrefix } from '../constants';

import { NotFoundError } from '../../../Error/PageNotFound';

import globals from '../../../../Globals';

const prefixUrl = globals.urlPrefix + appPrefix;

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

  componentWillReceiveProps(nextProps) {
    if (
      nextProps.params.remoteSchemaName !== this.props.params.remoteSchemaName
    ) {
      Promise.all([
        this.props.dispatch(
          fetchRemoteSchema(nextProps.params.remoteSchemaName)
        ),
        this.props.dispatch({
          type: VIEW_REMOTE_SCHEMA,
          data: nextProps.params.remoteSchemaName,
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
    const currentRemoteSchema = this.props.allRemoteSchemas.find(
      r => r.name === this.props.params.remoteSchemaName
    );

    if (!currentRemoteSchema) {
      // throw a 404 exception
      throw new NotFoundError();
    }

    const styles = require('../RemoteSchema.scss');

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
        url: appPrefix + '/' + 'manage',
      },
    ];

    if (remoteSchemaName) {
      breadCrumbs.push({
        title: remoteSchemaName.trim(),
        url:
          appPrefix +
          '/' +
          'manage' +
          '/' +
          remoteSchemaName.trim() +
          '/' +
          'details',
      });
      breadCrumbs.push({
        title: 'details',
        url: '',
      });
    }

    const refreshText =
      'If your remote schema has changed, you need to refresh the GraphQL Engine metadata to query the modified schema';

    if (readOnlyMode) {
      delete tabInfo.modify;
    }

    const showReloadRemoteSchema =
      !readOnlyMode && remoteSchemaName && remoteSchemaName.length > 0 ? (
        <div className={styles.commonBtn + ' ' + styles.detailsRefreshButton}>
          <ReloadRemoteSchema
            {...this.props}
            remoteSchemaName={remoteSchemaName}
          />
          <ToolTip message={refreshText} ml="xs" />
        </div>
      ) : null;

    return (
      <div
        className={styles.view_stitch_schema_wrapper + ' ' + styles.addWrapper}
      >
        <CommonTabLayout
          appPrefix={appPrefix}
          currentTab="details"
          heading={remoteSchemaName}
          tabsInfo={tabInfo}
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
                {filterHeaders.length > 0 ? (
                  <tr>
                    <td>Headers</td>
                    <td>
                      {filterHeaders &&
                        filterHeaders
                          .filter(k => !!k.name)
                          .map((h, i) => [
                            <tr key={i}>
                              <td>
                                {h.name} :{' '}
                                {h.type === 'static'
                                  ? h.value
                                  : '<' + h.value + '>'}
                              </td>
                            </tr>,
                            i !== filterHeaders.length - 1 ? <hr /> : null,
                          ])}
                    </td>
                  </tr>
                ) : null}
                {/*
                <tr>
                  <td>Webhook</td>
                  <td>in-use/bypassed</td>
                </tr>
                */}
              </tbody>
            </table>
          </div>
          {showReloadRemoteSchema}
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
