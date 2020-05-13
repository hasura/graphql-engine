import React from 'react';
import AdhocEventContainer from '../../Containers/AdhocEventsContainer';
import { ComponentReduxConnector } from '../../../../../Types';
import PendingEvents from './PendingEvents';

type Props = {
  dispatch: any;
};

const PendingEventsContainer: React.FC<Props> = props => {
  const { dispatch } = props;
  return (
    <AdhocEventContainer tabName="pending" dispatch={dispatch}>
      <PendingEvents dispatch={dispatch} />
    </AdhocEventContainer>
  );
};

const connector: ComponentReduxConnector = connect =>
  connect()(PendingEventsContainer);

export default connector;
