import React from 'react';
import STContainer from '../../Containers/ScheduledTriggerContainer';
import { Triggers } from '../../types';
import { MapReduxToProps, ComponentReduxConnector, Dispatch } from '../../../../../types';
import ProcessedEvents from './ProcessedEvents';

type Props = {
  allTriggers: Triggers;
  params?: {
    triggerName: string;
  };
  dispatch: Dispatch;
  readOnlyMode: boolean;
};

const ProcessedEventsContainer: React.FC<Props> = props => {
  const { dispatch, allTriggers, params } = props;
  const triggerName = params ? params.triggerName : '';
  return (
    <STContainer
      tabName="processed"
      dispatch={dispatch}
      triggerName={triggerName}
      allTriggers={allTriggers}
    >
      <ProcessedEvents dispatch={dispatch} />
    </STContainer>
  );
};

const mapStateToProps: MapReduxToProps = state => {
  return {
    allTriggers: state.events.triggers,
    readOnlyMode: state.main.readOnlyMode,
  };
};

const connector: ComponentReduxConnector = (connect: any) =>
  connect(mapStateToProps)(ProcessedEventsContainer);

export default connector;
