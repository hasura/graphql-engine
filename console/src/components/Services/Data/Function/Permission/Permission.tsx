import React, { useState, useEffect, useMemo } from 'react';
import Helmet from 'react-helmet';
import { connect, ConnectedProps } from 'react-redux';
import { Link, RouteComponentProps } from 'react-router';
import { push } from 'react-router-redux';

import globals from '../../../../../Globals';
import { ReduxState } from '../../../../../types';
import CommonTabLayout from '../../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import { mapDispatchToPropsEmpty } from '../../../../Common/utils/reactUtils';
import {
  getFunctionBaseRoute,
  getSchemaBaseRoute,
  getTablePermissionsRoute,
} from '../../../../Common/utils/routesUtils';
import { NotFoundError } from '../../../../Error/PageNotFound';
import {
  fetchFunctionInit,
  setTable,
  updateSchemaInfo,
  UPDATE_CURRENT_SCHEMA,
} from '../../DataActions';
import { fetchCustomFunction } from '../customFunctionReducer';
import tabInfo from '../Modify/tabInfo';
import PermissionsEditor from './PermissionsEditor';

import styles from '../Modify/ModifyCustomFunction.scss';
import { PGFunction } from '../../../../../dataSources/services/postgresql/types';
import { getFunctionSelector } from '../../../../../metadata/selector';

const PermissionServerFlagNote = ({ isEditable = false }) =>
  !isEditable ? (
    <>
      <br />
      <p>
        Function will be exposed automatically if there are SELECT permissions
        for the role. To expose query functions to roles explicitly, set{' '}
        <code>HASURA_GRAPHQL_INFER_FUNCTION_PERMISSIONS=false</code> on the
        server (
        <a
          href="https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-functions.html#api-custom-functions"
          target="_blank"
          rel="noopener noreferrer"
        >
          Read More
        </a>
        )
      </p>
    </>
  ) : (
    <>
      <br />
      <p>
        The function will be exposed to the role if SELECT permissions are
        enabled and function permissions are enabled for the role.
      </p>
    </>
  );

interface PermissionsProps extends ReduxProps {}
const Permissions: React.FC<PermissionsProps> = ({
  currentDataSource,
  currentSchema,
  currentFunction,
  currentFunctionInfo,
  dispatch,
  functions,
  serverConfig,
  allFunctions,
}) => {
  const isPermissionsEditable: boolean = useMemo(() => {
    const databaseFunction: PGFunction | undefined = allFunctions.find(
      (f: PGFunction) =>
        f.function_name === currentFunction &&
        f.function_schema === currentSchema
    );

    const isFunctionExposedAsMutation =
      currentFunctionInfo(currentFunction, currentSchema)?.configuration
        ?.exposed_as === 'mutation' ?? false;

    if (
      databaseFunction?.function_type === 'VOLATILE' &&
      (isFunctionExposedAsMutation ||
        !serverConfig.is_function_permissions_inferred)
    ) {
      return true;
    }

    return !serverConfig.is_function_permissions_inferred;
  }, [
    allFunctions,
    currentFunction,
    currentFunctionInfo,
    currentSchema,
    serverConfig.is_function_permissions_inferred,
  ]);

  const [funcFetchCompleted, updateFunctionFetchState] = useState(false);
  const urlWithSource = `/data/${currentDataSource}`;
  const urlWithSchema = `/data/${currentDataSource}/schema/${currentSchema}`;
  const prefixURL = `${globals.urlPrefix}${urlWithSource}`;

  useEffect(() => {
    if (!currentFunction) {
      dispatch(push(prefixURL));
    }
    dispatch(
      fetchCustomFunction(currentFunction, currentSchema, currentDataSource)
    ).then(() => {
      updateFunctionFetchState(true);
    });
  }, []);

  if (funcFetchCompleted && !currentFunction) {
    throw new NotFoundError();
  }

  const {
    functionSchema: schema,
    functionName,
    setOffTable,
    setOffTableSchema,
  } = functions;
  const functionBaseURL = getFunctionBaseRoute(
    schema,
    currentDataSource,
    functionName
  );
  const permissionTableURL = getTablePermissionsRoute(
    setOffTableSchema,
    currentDataSource,
    setOffTable,
    true
  );

  const breadCrumbs = [
    {
      title: 'Data',
      url: urlWithSchema,
    },
    {
      title: 'Schema',
      url: urlWithSchema,
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
      url: functionBaseURL,
    });
    breadCrumbs.push({
      title: 'Permission',
      url: '',
    });
  }

  return (
    <div className={`col-xs-8 ${styles.modifyWrapper}`}>
      <Helmet title={`Permission Custom Function - ${functionName} | Hasura`} />
      <CommonTabLayout
        appPrefix={urlWithSource}
        currentTab="permissions"
        heading={functionName}
        tabsInfo={tabInfo}
        breadCrumbs={breadCrumbs}
        baseUrl={functionBaseURL}
        showLoader={false}
        testPrefix="functions"
      />
      <br />
      <p>
        Permissions will be inherited from the SELECT permissions of the
        referenced table (
        <Link
          to={permissionTableURL}
          data-test="custom-function-permission-link"
          onClick={onClickPerm}
        >
          <b>{setOffTable}</b>
        </Link>
        ) by default.
      </p>
      <PermissionServerFlagNote isEditable={isPermissionsEditable} />
      <br />
      <PermissionsEditor
        currentFunctionName={currentFunction}
        currentSchema={currentSchema}
        isPermissionsEditable={isPermissionsEditable}
      />
    </div>
  );
};

type OwnProps = RouteComponentProps<
  {
    functionName: string;
    schema: string;
  },
  unknown
>;

const mapStateToProps = (state: ReduxState, ownProps: OwnProps) => ({
  currentSchema: ownProps.params.schema,
  currentFunction: ownProps.params.functionName,
  currentDataSource: state.tables.currentDataSource,
  functions: state.functions,
  serverConfig: state.main?.serverConfig?.data ?? {},
  allFunctions: state.tables.postgresFunctions,
  currentFunctionInfo: getFunctionSelector(state),
});
const functionsPermissionsConnector = connect(
  mapStateToProps,
  mapDispatchToPropsEmpty
);
type ReduxProps = ConnectedProps<typeof functionsPermissionsConnector>;
const FunctionPermissions = functionsPermissionsConnector(Permissions);

export default FunctionPermissions;
