import React from 'react';
import Helmet from 'react-helmet';
import { Link } from 'react-router';
import { push } from 'react-router-redux';
import { connect } from 'react-redux';

import CommonTabLayout from '../../../../Common/Layout/CommonTabLayout/CommonTabLayout';
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
import { pageTitle } from '../Modify/ModifyCustomFunction';
import styles from '../Modify/ModifyCustomFunction.scss';

class Permission extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      funcFetchCompleted: false,
    };
    this.urlWithSource = `/data/${props.currentSource}`;
    this.urlWithSchema = `/data/${props.currentSource}/schema/${props.currentSchema}`;
    this.prefixUrl = globals.urlPrefix + this.urlWithSource;
  }

  componentDidMount() {
    const { functionName, schema } = this.props.params;

    if (!functionName) {
      this.props.dispatch(push(this.prefixUrl));
    }

    Promise.all([
      this.props
        .dispatch(
          fetchCustomFunction(functionName, schema, this.props.currentSource)
        )
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

    const { dispatch, currentSource } = this.props;

    const functionBaseUrl = getFunctionBaseRoute(
      schema,
      currentSource,
      functionName
    );
    const permissionTableUrl = getTablePermissionsRoute(
      setOffTableSchema,
      currentSource,
      setOffTable,
      true
    );

    const breadCrumbs = [
      {
        title: 'Data',
        url: this.urlWithSource,
      },
      {
        title: 'Schema',
        url: this.urlWithSchema,
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
          appPrefix={this.urlWithSource}
          currentTab="permissions"
          heading={functionName}
          tabsInfo={tabInfo}
          breadCrumbs={breadCrumbs}
          baseUrl={functionBaseUrl}
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
            onClick={onClickPerm}
          >
            here
          </Link>
          .
        </p>
      </div>
    );
  }
}

const mapStateToProps = state => ({
  currentSchema: state.tables.currentSchema,
});

const permissionConnector = connect(mapStateToProps);
const ConnectedPermission = permissionConnector(Permission);

export default ConnectedPermission;
