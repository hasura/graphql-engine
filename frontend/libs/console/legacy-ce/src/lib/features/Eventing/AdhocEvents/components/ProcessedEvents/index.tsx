import React from 'react';
import AdhocEventContainer from '../Container';
import ProcessedEvents from './ProcessedEvents';

const AdhocEventProcessedEvents: React.FC = () => {
  return (
    <AdhocEventContainer tabName="processed">
      <ProcessedEvents />
    </AdhocEventContainer>
  );
};

export default AdhocEventProcessedEvents;
