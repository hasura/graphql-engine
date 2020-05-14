import React from 'react';
import AdhocEventContainer from '../../Containers/AdhocEventsContainer';
import { ComponentReduxConnector, MapReduxToProps, Dispatch } from '../../../../../types';
import ProcessedEvents from './ProcessedEvents';

type Props = {
  dispatch: Dispatch;
};

const ProcessedEventsContainer: React.FC<Props> = props => {
  const { dispatch } = props;
  return (
    <AdhocEventContainer tabName="processed" dispatch={dispatch}>
      <ProcessedEvents dispatch={dispatch} />
    </AdhocEventContainer>
  );
};

const mapStateToProps: MapReduxToProps = () => ({})

const connector: ComponentReduxConnector = connect =>
  connect(mapStateToProps)(ProcessedEventsContainer);

export default connector;
