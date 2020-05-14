import React from 'react';
import AdhocEventContainer from '../../Containers/AdhocEventsContainer';
import { ComponentReduxConnector, MapReduxToProps, Dispatch } from '../../../../../types';
import PendingEvents from './PendingEvents';

type Props = {
  dispatch: Dispatch;
};

const PendingEventsContainer: React.FC<Props> = props => {
  const { dispatch } = props;
  return (
    <AdhocEventContainer tabName="pending" dispatch={dispatch}>
      <PendingEvents dispatch={dispatch} />
    </AdhocEventContainer>
  );
};


const mapStateToProps: MapReduxToProps = () => ({})

const connector: ComponentReduxConnector = connect =>
  connect(mapStateToProps)(PendingEventsContainer);

export default connector;
