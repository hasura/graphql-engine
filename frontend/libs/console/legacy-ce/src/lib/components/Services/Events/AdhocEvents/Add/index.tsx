import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { getAdhocPendingEventsRoute } from '../../../../Common/utils/routesUtils';
import { Form } from '../../../../../features/AdhocEvents/components/Form';
import { mapDispatchToPropsEmpty } from '../../../../Common/utils/reactUtils';
import { MapStateToProps } from '../../../../../types';
import AdhocEventsContainer from '../Container';
import _push from '../../../Data/push';

type Props = InjectedProps;

const AddContainer: React.FC<Props> = ({ dispatch, readOnlyMode }) => {
  const onSuccess = () => {
    dispatch(_push(getAdhocPendingEventsRoute('absolute')));
  };
  return (
    <div className="bootstrap-jail">
      <AdhocEventsContainer tabName="add" dispatch={dispatch}>
        {readOnlyMode ? (
          'Cannot schedule event in read only mode'
        ) : (
          <div className="w-1/2">
            <Form onSuccess={onSuccess} />
          </div>
        )}
      </AdhocEventsContainer>
    </div>
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
