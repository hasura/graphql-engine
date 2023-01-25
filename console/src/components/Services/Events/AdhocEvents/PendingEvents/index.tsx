import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import AdhocEventContainer from '../Container';
import { mapDispatchToPropsEmpty } from '../../../../Common/utils/reactUtils';
import PendingEvents from './PendingEvents';

interface Props extends InjectedProps {}

const PendingEventsContainer: React.FC<Props> = props => {
  const { dispatch } = props;
  return (
    <AdhocEventContainer tabName="pending" dispatch={dispatch}>
      <PendingEvents dispatch={dispatch} />
    </AdhocEventContainer>
  );
};

const connector = connect(null, mapDispatchToPropsEmpty);

type InjectedProps = ConnectedProps<typeof connector>;

const PendingEventsConnector = connector(PendingEventsContainer);
export default PendingEventsConnector;
