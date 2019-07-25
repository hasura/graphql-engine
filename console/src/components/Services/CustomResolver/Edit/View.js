import React from 'react';

import CommonTabLayout from '../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import tabInfo from './tabInfo';
import Tooltip from 'react-bootstrap/lib/Tooltip';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import { push } from 'react-router-redux';

import {
  fetchResolver,
  RESET,
  getHeaderEvents,
} from '../Add/addResolverReducer';

import { VIEW_RESOLVER } from '../customActions';
import ReloadRemoteSchema from '../../Metadata/MetadataOptions/ReloadRemoteSchema';

import { appPrefix } from '../constants';

import { NotFoundError } from '../../../Error/PageNotFound';

import globals from '../../../../Globals';

const prefixUrl = globals.urlPrefix + appPrefix;

class ViewStitchedSchema extends React.Component {
  componentDidMount() {
    const { resolverName } = this.props.params;
    if (!resolverName) {
      this.props.dispatch(push(prefixUrl));
    }
    Promise.all([
      this.props.dispatch(fetchResolver(resolverName)),
      this.props.dispatch({ type: VIEW_RESOLVER, data: resolverName }),
    ]);
  }

  componentWillReceiveProps(nextProps) {
    if (nextProps.params.resolverName !== this.props.params.resolverName) {
      Promise.all([
        this.props.dispatch(fetchResolver(nextProps.params.resolverName)),
        this.props.dispatch({
          type: VIEW_RESOLVER,
          data: nextProps.params.resolverName,
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
      this.props.dispatch({ type: VIEW_RESOLVER, data: '' }),
    ]);
  }

  render() {
    const currentResolver = this.props.allResolvers.find(
      r => r.name === this.props.params.resolverName
    );

    if (!currentResolver) {
      // throw a 404 exception
      throw new NotFoundError();
    }

    const styles = require('../CustomResolver.scss');

    const { resolverName } = this.props.params;
    const { manualUrl, envName, headers } = this.props;

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

    if (resolverName) {
      breadCrumbs.push({
        title: resolverName.trim(),
        url:
          appPrefix +
          '/' +
          'manage' +
          '/' +
          resolverName.trim() +
          '/' +
          'details',
      });
      breadCrumbs.push({
        title: 'details',
        url: '',
      });
    }

    const refresh = (
      <Tooltip id="tooltip-cascade">
        If your remote schema has changed, you need to refresh the GraphQL
        Engine metadata to query the modified schema
      </Tooltip>
    );

    const showReloadRemoteSchema =
      resolverName && resolverName.length > 0 ? (
        <div className={styles.commonBtn + ' ' + styles.detailsRefreshButton}>
          <span>
            <ReloadRemoteSchema
              {...this.props}
              remoteSchemaName={resolverName}
            />
          </span>
          <span>
            <OverlayTrigger placement="right" overlay={refresh}>
              <i className="fa fa-question-circle" aria-hidden="true" />
            </OverlayTrigger>
          </span>
        </div>
      ) : null;

    return (
      <div
        className={styles.view_stitch_schema_wrapper + ' ' + styles.addWrapper}
      >
        <CommonTabLayout
          appPrefix={appPrefix}
          currentTab="details"
          heading={resolverName}
          tabsInfo={tabInfo}
          breadCrumbs={breadCrumbs}
          baseUrl={`${appPrefix}/manage/${resolverName}`}
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
    ...state.customResolverData.addData,
    ...state.customResolverData.headerData,
    allResolvers: state.customResolverData.listData.resolvers,
    dataHeaders: { ...state.tables.dataHeaders },
  };
};

export default connect => connect(mapStateToProps)(ViewStitchedSchema);
