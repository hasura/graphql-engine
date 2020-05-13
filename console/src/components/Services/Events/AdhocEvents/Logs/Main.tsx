import React from 'react';
import AdhocEventsContainer from '../../Containers/AdhocEventsContainer';
import { ComponentReduxConnector } from '../../../../../Types';
import Logs from './Logs';

type LogsProps = {
  dispatch: any;
};

const LogsContainer = ({ dispatch }: LogsProps) => {
  return (
    <AdhocEventsContainer tabName="logs" dispatch={dispatch}>
      <Logs dispatch={dispatch} />
    </AdhocEventsContainer>
  );
};

const connector: ComponentReduxConnector = connect => connect()(LogsContainer);
export default connector;
