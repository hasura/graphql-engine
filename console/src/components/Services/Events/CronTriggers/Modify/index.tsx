import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import STContainer from '../Container';
import { Triggers, RouterTriggerProps } from '../../types';
import { MapStateToProps } from '../../../../../types';
import Modify from './Modify';
import { mapDispatchToPropsEmpty } from '../../../../Common/utils/reactUtils';

interface Props extends InjectedProps {}

const ModifyContainer: React.FC<Props> = props => {
  const { dispatch, allTriggers, triggerName, readOnlyMode } = props;
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

type PropsFromState = {
  allTriggers: Triggers;
  triggerName: string;
  readOnlyMode: boolean;
};

const mapStateToProps: MapStateToProps<PropsFromState, RouterTriggerProps> = (
  state,
  ownProps
) => {
  return {
    allTriggers: state.events.triggers,
    readOnlyMode: state.main.readOnlyMode,
    triggerName: ownProps.params.triggerName,
  };
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);

type InjectedProps = ConnectedProps<typeof connector>;

export default connector(ModifyContainer);
