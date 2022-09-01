import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { mapDispatchToPropsEmpty } from '../../../../Common/utils/reactUtils';
import AdhocEventContainer from '../Container';
import ProcessedEvents from './ProcessedEvents';

interface Props extends InjectedProps {}

const ProcessedEventsContainer: React.FC<Props> = props => {
  const { dispatch } = props;
  return (
    <AdhocEventContainer tabName="processed" dispatch={dispatch}>
      <ProcessedEvents dispatch={dispatch} />
    </AdhocEventContainer>
  );
};

const connector = connect(null, mapDispatchToPropsEmpty);

type InjectedProps = ConnectedProps<typeof connector>;

const ProcessedEventsConnector = connector(ProcessedEventsContainer);
export default ProcessedEventsConnector;
