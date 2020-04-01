import React from 'react';

import Helmet from 'react-helmet';
import CommonTabLayout from '../../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import { Link } from 'react-router';
import { push } from 'react-router-redux';

import { pageTitle, appPrefix } from '../Modify/constants';

import tabInfo from '../Modify/tabInfo';
import globals from '../../../../../Globals';

import { fetchCustomFunction } from '../customFunctionReducer';
import {
  updateSchemaInfo,
  UPDATE_CURRENT_SCHEMA,
  fetchFunctionInit,
  setTable,
} from '../../DataActions';
import { NotFoundError } from '../../../../Error/PageNotFound';
import {
  getSchemaBaseRoute,
  getFunctionBaseRoute,
  getTablePermissionsRoute,
} from '../../../../Common/utils/routesUtils';
import { Text } from '../../../../UIKit/atoms';
import styles from '../Modify/ModifyCustomFunction.scss';

const prefixUrl = globals.urlPrefix + appPrefix;

class Permission extends React.Component {
  constructor() {
    super();

    this.state = {
      funcFetchCompleted: false,
    };
  }

  componentDidMount() {
    const { functionName, schema } = this.props.params;

    if (!functionName) {
      this.props.dispatch(push(prefixUrl));
    }

    Promise.all([
      this.props
        .dispatch(fetchCustomFunction(functionName, schema))
        .then(() => {
          this.setState({ funcFetchCompleted: true });
        }),
    ]);
  }

  render() {
    const {
      functionSchema: schema,
      functionName,
      setOffTable,
      setOffTableSchema,
    } = this.props.functions;

    if (this.state.funcFetchCompleted && !functionName) {
      // throw a 404 exception
      throw new NotFoundError();
    }

    const { dispatch } = this.props;

    const functionBaseUrl = getFunctionBaseRoute(schema, functionName);
    const permissionTableUrl = getTablePermissionsRoute(
      setOffTableSchema,
      setOffTable,
      true
    );

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
        url: getSchemaBaseRoute(schema),
      },
    ];

    const onClickPerm = () => {
      if (schema !== setOffTableSchema) {
        Promise.all([
          dispatch({
            type: UPDATE_CURRENT_SCHEMA,
            currentSchema: setOffTableSchema,
          }),
          dispatch(updateSchemaInfo()),
          dispatch(fetchFunctionInit()),
          dispatch(setTable(setOffTable)),
        ]);
      }
    };

    if (functionName) {
      breadCrumbs.push({
        title: functionName,
        url: functionBaseUrl,
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
          baseUrl={functionBaseUrl}
          showLoader={false}
          testPrefix={'functions'}
        />
        <br />
        <Text>
          Permissions defined for the SETOF table, <b>{setOffTable}</b>, are
          applicable to the data returned by this function.
          <br />
          <br />
          See <b>{setOffTable}</b> permissions{' '}
          <Link
            to={permissionTableUrl}
            data-test="custom-function-permission-link"
            onClick={onClickPerm}
          >
            here
          </Link>
          .
        </Text>
      </div>
    );
  }
}

export default Permission;
