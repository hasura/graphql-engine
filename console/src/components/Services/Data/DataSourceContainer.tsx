import React, { useEffect, useState } from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { ThunkDispatch } from 'redux-thunk';
import { RouteComponentProps } from 'react-router';
import { push } from 'react-router-redux';
import { AnyAction } from 'redux';

import { ReduxState } from '../../../types';
import { getDataSources, getInitDataSource } from '../../../metadata/selector';
import { showErrorNotification } from '../Common/Notification';
import {
  fetchDataInit,
  fetchFunctionInit,
  // updateSchemaInfo,
  UPDATE_CURRENT_DATA_SOURCE,
  UPDATE_CURRENT_SCHEMA,
} from './DataActions';
import { setDriver } from '../../../dataSources/dataSources';

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
  driver,
  location,
}: DataSourceContainerProps) => {
  const [dataLoaded, setDataLoaded] = useState(false);
  const { source, schema } = params;

  useEffect(() => {
    if (!source || source === 'undefined') {
      if (currentSource) {
        dispatch(push(`/data/${currentSource}`));
        return;
      }

      const newSource = dataSources.length ? dataSources[0].name : '';
      dispatch({ type: UPDATE_CURRENT_DATA_SOURCE, source: newSource });
      dispatch(push(`/data/${newSource}`));
      return;
    }

    if (source === currentSource) {
      return;
    }

    if (!dataSources.find(s => s.name === source)) {
      dispatch(push('/data/manage'));
      dispatch(
        showErrorNotification(`Data source "${source}" doesn't exist`, null)
      );
      return;
    }
    dispatch({ type: UPDATE_CURRENT_DATA_SOURCE, source });
  }, [currentSource, dataSources, dispatch, source]);

  useEffect(() => {
    if (!source || source === 'undefined') return;

    if (schema) {
      if (schemaList.find((s: string) => s === schema)) {
        dispatch({ type: UPDATE_CURRENT_SCHEMA, currentSchema: schema });
      } else {
        dispatch(
          showErrorNotification(`Schema "${schema}" doesn't exist`, null)
        );
        dispatch(push(`/data/${source}`));
      }
      return;
    }

    let newSchema = '';
    if (schemaList.length) {
      newSchema = schemaList.includes('public') ? 'public' : schemaList[0];
    }
    dispatch({ type: UPDATE_CURRENT_SCHEMA, currentSchema: newSchema });
    if (location.pathname.includes('schema')) {
      dispatch(push(`/data/${source}/schema/${newSchema}`));
    }
  }, [dispatch, schema, schemaList, source, location]);

  useEffect(() => {
    setDriver(driver);
  }, [driver]);

  useEffect(() => {
    if (currentSource) {
      dispatch(fetchDataInit()).then(() => {
        dispatch(fetchFunctionInit());
        setDataLoaded(true);
      });
    }
  }, [currentSource, dataLoaded, dispatch]);

  if (!currentSource || !dataLoaded) {
    return <div style={{ margin: '20px' }}>Loading...</div>;
  }

  return <>{children}</>;
};

const mapStateToProps = (state: ReduxState) => {
  return {
    schemaList: state.tables.schemaList,
    dataSources: getDataSources(state),
    currentSource: state.tables.currentDataSource,
    driver: getInitDataSource(state).driver,
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
