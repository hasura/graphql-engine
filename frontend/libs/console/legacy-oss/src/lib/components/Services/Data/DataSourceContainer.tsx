import React, { useEffect, useState } from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { ThunkDispatch } from 'redux-thunk';
import { RouteComponentProps } from 'react-router';
import { AnyAction } from 'redux';

import _push from './push';
import { ReduxState } from '../../../types';
import { getDataSources } from '../../../metadata/selector';
import { showErrorNotification } from '../Common/Notification';
import {
  fetchDataInit,
  fetchFunctionInit,
  UPDATE_CURRENT_DATA_SOURCE,
  UPDATE_CURRENT_SCHEMA,
} from './DataActions';
import { currentDriver, useDataSource } from '../../../dataSources';
import SourceView from './SourceView';
import { getSourceDriver, isInconsistentSource } from './utils';
import {
  findTable,
  generateTableDef,
  getSchemaTables,
} from '../../../dataSources/common';
import { isTableRoute } from '../../Common/utils/routesUtils';

type Params = {
  source?: string;
  schema?: string;
};

interface DataSourceContainerProps
  extends DataSourceContainerInjectedProps,
    RouteComponentProps<Params, Params>,
    React.ComponentProps<'div'> {}
const DataSourceContainer = ({
  children,
  params,
  schemaList,
  dataSources,
  dispatch,
  currentSource,
  location,
  inconsistentObjects,
  currentSchema,
  currentTable,
  allSchemas,
}: DataSourceContainerProps) => {
  const { setDriver, dataSource } = useDataSource();
  const [dataLoaded, setDataLoaded] = useState(false);
  const { source, schema } = params;

  useEffect(() => {
    // if the source is inconsistent, do not show the source route
    if (isInconsistentSource(currentSource, inconsistentObjects)) {
      dispatch(_push('/data/manage'));
    }
  }, [inconsistentObjects, currentSource, dispatch, location]);

  useEffect(() => {
    if (!source || source === 'undefined') {
      if (currentSource) {
        dispatch(_push(`/data/${currentSource}`));
        return;
      }

      const newSource = dataSources.length ? dataSources[0].name : '';
      setDriver(getSourceDriver(dataSources, newSource));
      dispatch({ type: UPDATE_CURRENT_DATA_SOURCE, source: newSource });
      dispatch(_push(`/data/${newSource}`));
      return;
    }

    setDriver(getSourceDriver(dataSources, source));
    dispatch({ type: UPDATE_CURRENT_DATA_SOURCE, source });
    if (source === currentSource) {
      return;
    }

    if (!dataSources.find(s => s.name === source)) {
      dispatch(_push('/data/manage'));
      dispatch(
        showErrorNotification(`Data source "${source}" doesn't exist`, null)
      );
    }
  }, [currentSource, dataSources, dispatch, source]);

  useEffect(() => {
    if (!source || source === 'undefined') return;

    if (schema) {
      dispatch({ type: UPDATE_CURRENT_SCHEMA, currentSchema: schema });
      return;
    }
    // eslint-disable-next-line no-useless-return
    if (!dataLoaded) return;

    let newSchema = '';

    if (schemaList.length) {
      newSchema =
        dataSource.defaultRedirectSchema &&
        schemaList.includes(dataSource.defaultRedirectSchema)
          ? dataSource.defaultRedirectSchema
          : schemaList.sort(Intl.Collator().compare)[0];
    }
    if (location.pathname.includes('schema') && newSchema) {
      dispatch(_push(`/data/${source}/schema/${newSchema}`));
    }
  }, [dispatch, schema, schemaList, location, source, dataLoaded]);

  useEffect(() => {
    const driver = getSourceDriver(dataSources, currentSource);
    if (driver !== currentDriver) return;
    if (schemaList?.length > 0) return setDataLoaded(true);

    if (currentSource) {
      dispatch(fetchDataInit(currentSource, currentDriver)).then(() => {
        dispatch(fetchFunctionInit()); // todo
        setDataLoaded(true);
      });
    }
  }, [currentSource, dataLoaded, dispatch, currentDriver]);

  if (!currentSource || !dataLoaded) {
    return <div style={{ margin: '20px' }}>Loading data...</div>;
  }

  if (isTableRoute(location.pathname) && currentSchema) {
    const table = findTable(
      allSchemas,
      generateTableDef(currentTable, currentSchema)
    );

    if (!table) {
      // TODO: detect more accurately if table data is still loading
      // HACK: if schemaTables are present => schema data has been loaded at some point,
      // hence if table is not found => 404
      // This condition will always pass if schema has no tables or in case we have stale
      // data that doesnt have the current table data
      const schemaTables = getSchemaTables(allSchemas, currentSchema);
      if (schemaTables.length === 0) {
        return <div style={{ margin: '20px' }}>Loading data...</div>;
      }
    }
  }

  return <>{children || <SourceView />}</>;
};

const mapStateToProps = (state: ReduxState) => {
  return {
    schemaList: state.tables.schemaList,
    dataSources: getDataSources(state),
    allSchemas: state.tables.allSchemas,
    currentSchema: state.tables.currentSchema,
    currentTable: state.tables.currentTable,
    currentSource: state.tables.currentDataSource,
    inconsistentObjects: state.metadata.inconsistentObjects,
  };
};
const dataSourceConnector = connect(
  mapStateToProps,
  (dispatch: ThunkDispatch<ReduxState, unknown, AnyAction>) => ({
    dispatch,
  })
);

type DataSourceContainerInjectedProps = ConnectedProps<
  typeof dataSourceConnector
>;

const ConnectedDataSourceContainer = dataSourceConnector(DataSourceContainer);
export default ConnectedDataSourceContainer;
