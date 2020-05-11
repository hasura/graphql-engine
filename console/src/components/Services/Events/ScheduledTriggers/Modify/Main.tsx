import React from 'react';
import STContainer from '../../Containers/ScheduledTriggerContainer';
import { Triggers } from '../../Types';
import { MapReduxToProps, ComponentReduxConnector } from '../../../../../Types';
import Modify from './Modify';

type Props = {
  allTriggers: Triggers;
  params?: {
    triggerName: string;
  };
  dispatch: any;
  readOnlyMode: boolean;
};

const ModifyContainer: React.FC<Props> = props => {
  const { dispatch, allTriggers, params, readOnlyMode } = props;
  const triggerName = params ? params.triggerName : '';
  return (
    <STContainer
      tabName="modify"
      dispatch={dispatch}
      triggerName={triggerName}
      allTriggers={allTriggers}
    >
      {readOnlyMode ? (
        'Cannot modify in read-only mode'
      ) : (
        <Modify dispatch={dispatch} />
      )}
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
  connect(mapStateToProps)(ModifyContainer);

export default connector;
