import React from 'react';
import AdhocEventsContainer from '../../Containers/AdhocEventsContainer';
import { ComponentReduxConnector, MapReduxToProps, Dispatch } from '../../../../../types';
import Add from './Add';

type Props = {
  dispatch: Dispatch;
};

const AddContainer = ({ dispatch }: Props) => {
  return (
    <AdhocEventsContainer tabName="add" dispatch={dispatch}>
      <Add dispatch={dispatch} />
    </AdhocEventsContainer>
  );
};

const mapStateToProps: MapReduxToProps = () => ({})

const connector: ComponentReduxConnector = connect => connect(mapStateToProps)(AddContainer);
export default connector;
