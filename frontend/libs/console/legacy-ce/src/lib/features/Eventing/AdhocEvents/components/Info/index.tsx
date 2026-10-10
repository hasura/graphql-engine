import React from 'react';
import AdhocEventsContainer from '../Container';
import Info from './Info';

const AdhocEventsInfo: React.FC = () => {
  return (
    <AdhocEventsContainer tabName="info">
      <Info />
    </AdhocEventsContainer>
  );
};

export default AdhocEventsInfo;
