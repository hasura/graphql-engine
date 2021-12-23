import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import STContainer from '../Container';
import { Triggers, RouterTriggerProps } from '../../types';
import { MapStateToProps } from '../../../../../types';
import ProcessedEvents from './ProcessedEvents';
import { mapDispatchToPropsEmpty } from '../../../../Common/utils/reactUtils';
import { getCronTriggers } from '../../../../../metadata/selector';

interface Props extends InjectedProps {}

const ProcessedEventsContainer: React.FC<Props> = props => {
  const { dispatch, allTriggers, triggerName } = props;
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

const ProcessedEventsConnector = connector(ProcessedEventsContainer);
export default ProcessedEventsConnector;
