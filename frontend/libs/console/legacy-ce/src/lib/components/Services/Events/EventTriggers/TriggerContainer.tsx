import React, { useEffect } from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { setDriver } from '../../../../dataSources';
import {
  getDataSources,
  getEventTriggerByName,
} from '../../../../metadata/selector';
import { ReduxState } from '../../../../types';
import { UPDATE_CURRENT_DATA_SOURCE } from '../../Data/DataActions';
import { getSourceDriver } from '../../Data/utils';
import { RouterTriggerProps } from '../types';

const TriggerContainer: React.FC<Props> = ({
  children,
  currentTrigger,
  dispatch,
  currentDataSource,
  driver,
}) => {
  useEffect(() => {
    if (currentTrigger && currentTrigger.source !== currentDataSource) {
      dispatch({
        type: UPDATE_CURRENT_DATA_SOURCE,
        source: currentTrigger?.source,
      });
      setDriver(driver);
    }
  }, [currentDataSource, currentTrigger, dispatch]);

  if (!currentTrigger) return null;

  return <>{children}</>;
};

const mapStateToProps = (state: ReduxState, ownProps: RouterTriggerProps) => {
  const triggerName = ownProps.params.triggerName;
  const currentTrigger = getEventTriggerByName(state)(triggerName);
  const driver = getSourceDriver(getDataSources(state), currentTrigger?.source);

  return {
    currentTrigger,
    currentDataSource: state.tables.currentDataSource,
    driver,
  };
};

const connector = connect(mapStateToProps);
type Props = ConnectedProps<typeof connector>;

export const TriggerContainerConnector = connector(TriggerContainer);
