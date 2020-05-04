import React from 'react';
import STContainer from '../../Containers/ScheduledTriggerContainer';
import { Triggers } from '../../Types';
import { ReduxState } from '../../../../../Types';
import ProcessedEvents from './ProcessedEvents';

type Props = {
  allTriggers: Triggers;
  params?: {
    triggerName: string;
  };
  dispatch: any;
  readOnlyMode: boolean;
};

const ProcessedEventsContainer = ({
  dispatch,
  allTriggers,
  params,
  readOnlyMode,
}: Props) => {
  const triggerName = params ? params.triggerName : '';
  return (
    <STContainer
      tabName="processed"
      dispatch={dispatch}
      triggerName={triggerName}
      allTriggers={allTriggers}
    >
      <ProcessedEvents dispatch={dispatch} readOnlyMode={readOnlyMode} />
    </STContainer>
  );
};

const mapStateToProps = (state: ReduxState) => {
  return {
    allTriggers: state.events.triggers,
    readOnlyMode: state.main.readOnlyMode,
  };
};

export default (connect: any) =>
  connect(mapStateToProps)(ProcessedEventsContainer);
