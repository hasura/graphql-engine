import React from 'react';
import AdhocEventContainer from '../Container';
import PendingEvents from './PendingEvents';

const AdhocEventPendingEvents: React.FC = () => {
  return (
    <AdhocEventContainer tabName="pending">
      <PendingEvents />
    </AdhocEventContainer>
  );
};

export default AdhocEventPendingEvents;
