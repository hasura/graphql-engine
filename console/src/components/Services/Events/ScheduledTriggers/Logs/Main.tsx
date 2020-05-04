import React from 'react';
import STContainer from '../../Containers/ScheduledTriggerContainer';
import { Triggers } from '../../Types';
import { ReduxState } from '../../../../../Types';
import Logs from './Logs';

type LogsProps = {
  allTriggers: Triggers;
  params?: {
    triggerName: string;
  };
  dispatch: any;
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

const mapStateToProps = (state: ReduxState) => {
  return {
    allTriggers: state.events.triggers,
  };
};

export default (connect: any) => connect(mapStateToProps)(LogsContainer);
