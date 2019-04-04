import React from 'react';

import Helmet from 'react-helmet';
import CommonTabLayout from '../../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import { Link } from 'react-router';
import { push } from 'react-router-redux';

import { pageTitle, appPrefix } from '../Modify/constants';

import tabInfo from '../Modify/tabInfo';
import globals from '../../../../../Globals';

const prefixUrl = globals.urlPrefix + appPrefix;

import { fetchCustomFunction } from '../customFunctionReducer';

class Permission extends React.Component {
  componentDidMount() {
    const { functionName, schema } = this.props.params;
    if (!functionName) {
      this.props.dispatch(push(prefixUrl));
    }
    Promise.all([
      this.props.dispatch(fetchCustomFunction(functionName, schema)),
    ]);
  }
  render() {
    const styles = require('../Modify/ModifyCustomFunction.scss');
    const {
      functionSchema: schema,
      functionName,
      setOffTable,
      setOffTableSchema,
    } = this.props.functions;

    const baseUrl = `${appPrefix}/schema/${schema}/functions/${functionName}`;
    const permissionTableUrl = `${prefixUrl}/schema/${setOffTableSchema}/tables/${setOffTable}/permissions`;

    const breadCrumbs = [
      {
        title: 'Data',
        url: appPrefix,
      },
      {
        title: 'Schema',
        url: appPrefix + '/schema',
      },
      {
        title: schema,
        url: appPrefix + '/schema/' + schema,
      },
    ];

    if (functionName) {
      breadCrumbs.push({
        title: functionName,
        url: appPrefix + '/schema/' + schema + '/functions/' + functionName,
      });
      breadCrumbs.push({
        title: 'Permission',
        url: '',
      });
    }
    return (
      <div className={'col-xs-8' + ' ' + styles.modifyWrapper}>
        <Helmet
          title={`Permission ${pageTitle} - ${functionName} - ${pageTitle}s | Hasura`}
        />
        <CommonTabLayout
          appPrefix={appPrefix}
          currentTab="permissions"
          heading={functionName}
          tabsInfo={tabInfo}
          breadCrumbs={breadCrumbs}
          baseUrl={baseUrl}
          showLoader={false}
          testPrefix={'functions'}
        />
        <br />
        <p>
          Permissions defined for the SETOF table, <b>{setOffTable}</b>, are
          applicable to the data returned by this function.
          <br />
          <br />
          See <b>{setOffTable}</b> permissions{' '}
          <Link
            to={permissionTableUrl}
            data-test="custom-function-permission-link"
          >
            here
          </Link>
          .
        </p>
      </div>
    );
  }
}

export default Permission;
