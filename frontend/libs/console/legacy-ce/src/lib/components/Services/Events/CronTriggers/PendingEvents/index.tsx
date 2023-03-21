import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import STContainer from '../Container';
import { RouterTriggerProps } from '../../types';
import { MapStateToProps } from '../../../../../types';
import PendingEvents from './PendingEvents';
import { mapDispatchToPropsEmpty } from '../../../../Common/utils/reactUtils';
import { useGetCronTriggers } from '../Hooks/useGetCronTriggers';

type Props = InjectedProps;

const PendingEventsContainer: React.FC<Props> = props => {
  const { data: cronTriggers, isLoading, error } = useGetCronTriggers();
  if (isLoading) {
    return <span>Loading...</span>;
  }
  if (!cronTriggers) {
    return <span>Could not find any cron triggers</span>;
  }
  if (error) {
    return <span>There was an error, please try again later</span>;
  }
  const { dispatch, triggerName } = props;
  return (
    <STContainer
      tabName="pending"
      dispatch={dispatch}
      triggerName={triggerName}
      allTriggers={cronTriggers}
    >
      <PendingEvents dispatch={dispatch} />
    </STContainer>
  );
};

type PropsFromState = {
  triggerName: string;
};

const mapStateToProps: MapStateToProps<PropsFromState, RouterTriggerProps> = (
  state,
  ownProps
) => {
  return {
    triggerName: ownProps.params.triggerName,
  };
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);

type InjectedProps = ConnectedProps<typeof connector>;

const PendingEventsConnector = connector(PendingEventsContainer);
export default PendingEventsConnector;
