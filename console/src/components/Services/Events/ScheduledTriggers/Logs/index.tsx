import React from 'react';
import STContainer from '../../Containers/ScheduledTriggerContainer';
import { Triggers } from '../../types';
import { MapReduxToProps, ComponentReduxConnector, Dispatch } from '../../../../../types';
import Logs from './Logs';

type LogsProps = {
  allTriggers: Triggers;
  params?: {
    triggerName: string;
  };
  dispatch: Dispatch;
};

const LogsContainer = ({ dispatch, allTriggers, params }: LogsProps) => {
  const triggerName = params ? params.triggerName : '';
  return (
    <STContainer
      tabName="logs"
      dispatch={dispatch}
      triggerName={triggerName}
      allTriggers={allTriggers}
    >
      <Logs dispatch={dispatch} />
    </STContainer>
  );
};

const mapStateToProps: MapReduxToProps = state => {
  return {
    allTriggers: state.events.triggers,
  };
};

const connector: ComponentReduxConnector = (connect: any) =>
  connect(mapStateToProps)(LogsContainer);
export default connector;
