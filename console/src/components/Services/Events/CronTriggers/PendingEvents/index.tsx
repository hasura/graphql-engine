import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import STContainer from '../Container';
import { Triggers, RouterTriggerProps } from '../../types';
import { MapStateToProps } from '../../../../../types';
import PendingEvents from './PendingEvents';
import { mapDispatchToPropsEmpty } from '../../../../Common/utils/reactUtils';
import { getCronTriggers } from '../../../../../metadata/selector';

interface Props extends InjectedProps {}

const PendingEventsContainer: React.FC<Props> = props => {
  const { dispatch, allTriggers, triggerName } = props;
  return (
    <STContainer
      tabName="pending"
      dispatch={dispatch}
      triggerName={triggerName}
      allTriggers={allTriggers}
    >
      <PendingEvents dispatch={dispatch} />
    </STContainer>
  );
};

type PropsFromState = {
  allTriggers: Triggers;
  triggerName: string;
};

const mapStateToProps: MapStateToProps<PropsFromState, RouterTriggerProps> = (
  state,
  ownProps
) => {
  return {
    allTriggers: getCronTriggers(state),
    triggerName: ownProps.params.triggerName,
  };
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);

type InjectedProps = ConnectedProps<typeof connector>;

const PendingEventsConnector = connector(PendingEventsContainer);
export default PendingEventsConnector;
