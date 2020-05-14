import React from 'react';
import AdhocEventsContainer from '../../Containers/AdhocEventsContainer';
import { ComponentReduxConnector, MapReduxToProps, Dispatch } from '../../../../../types';
import Logs from './Logs';

type LogsProps = {
  dispatch: Dispatch;
};

const LogsContainer = ({ dispatch }: LogsProps) => {
  return (
    <AdhocEventsContainer tabName="logs" dispatch={dispatch}>
      <Logs dispatch={dispatch} />
    </AdhocEventsContainer>
  );
};

const mapStateToProps: MapReduxToProps = () => ({})

const connector: ComponentReduxConnector = connect => connect(mapStateToProps)(LogsContainer);
export default connector;
