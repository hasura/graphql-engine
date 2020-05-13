import React from 'react';
import AdhocEventsContainer from '../../Containers/AdhocEventsContainer';
import { ComponentReduxConnector } from '../../../../../Types';
import Add from './Add';

type Props = {
  dispatch: any;
};

const AddContainer = ({ dispatch }: Props) => {
  return (
    <AdhocEventsContainer tabName="add" dispatch={dispatch}>
      <Add dispatch={dispatch} />
    </AdhocEventsContainer>
  );
};

const connector: ComponentReduxConnector = connect => connect()(AddContainer);
export default connector;
