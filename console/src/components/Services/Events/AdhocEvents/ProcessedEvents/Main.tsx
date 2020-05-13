import React from 'react';
import AdhocEventContainer from '../../Containers/AdhocEventsContainer';
import { ComponentReduxConnector } from '../../../../../Types';
import ProcessedEvents from './ProcessedEvents';

type Props = {
  dispatch: any;
};

const ProcessedEventsContainer: React.FC<Props> = props => {
  const { dispatch } = props;
  return (
    <AdhocEventContainer tabName="processed" dispatch={dispatch}>
      <ProcessedEvents dispatch={dispatch} />
    </AdhocEventContainer>
  );
};

const connector: ComponentReduxConnector = connect =>
  connect()(ProcessedEventsContainer);

export default connector;
