import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { mapDispatchToPropsEmpty } from '../../../../Common/utils/reactUtils';
import { MapStateToProps } from '../../../../../types';
import AdhocEventsContainer from '../Container';
import Add from './Add';

interface Props extends InjectedProps {}

const AddContainer: React.FC<Props> = ({ dispatch, readOnlyMode }) => {
  return (
    <AdhocEventsContainer tabName="add" dispatch={dispatch}>
      {readOnlyMode ? (
        'Cannot schedule event in read only mode'
      ) : (
        <Add dispatch={dispatch} />
      )}
    </AdhocEventsContainer>
  );
};

type PropsFromState = {
  readOnlyMode: boolean;
};
const mapStateToProps: MapStateToProps<PropsFromState> = state => ({
  readOnlyMode: state.main.readOnlyMode,
});

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);

type InjectedProps = ConnectedProps<typeof connector>;

const AddConnector = connector(AddContainer);
export default AddConnector;
