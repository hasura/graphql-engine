import React from 'react';

import Helmet from 'react-helmet';
import CommonTabLayout from '../../../Layout/CommonTabLayout/CommonTabLayout';
import { Link } from 'react-router';
import { push } from 'react-router-redux';

import { pageTitle, appPrefix } from '../Modify/constants';

import tabInfo from '../Modify/tabInfo';
import globals from '../../../../../Globals';

const prefixUrl = globals.urlPrefix + appPrefix;

import { fetchCustomFunction } from '../customFunctionReducer';

class Permission extends React.Component {
  componentDidMount() {
    const { functionName } = this.props.params;
    if (!functionName) {
      this.props.dispatch(push(prefixUrl));
    }
    Promise.all([this.props.dispatch(fetchCustomFunction(functionName))]);
  }
  render() {
    const styles = require('../Modify/ModifyCustomFunction.scss');
    const {
      functionSchema: schema,
      functionName,
      setOffTable,
    } = this.props.functions;
    const baseUrl = `${appPrefix}/schema/${schema}/functions/${functionName}`;
    const permissionTableUrl = `${appPrefix}/schema/${schema}/tables/${setOffTable}/permissions`;

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
          Note: Permission defined for the setof table, {`${setOffTable}`}, are
          applicable to the data returned by this function
        </p>
        <div className={styles.commonBtn}>
          <Link to={permissionTableUrl}>
            <button
              className={styles.yellow_button}
              data-test={'custom-function-permission-btn'}
            >
              {`${setOffTable} Permissions`}
            </button>
          </Link>
        </div>
      </div>
    );
  }
}

export default Permission;
