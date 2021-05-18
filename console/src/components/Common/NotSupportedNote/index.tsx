import React from 'react';
import { Driver, currentDriver, driverToLabel } from '../../../dataSources';

type NotSupportedNoteProps = {
  unsupported: Driver[];
};
export const NotSupportedNote = ({ unsupported }: NotSupportedNoteProps) => {
  if (!currentDriver || !unsupported.length) {
    return null;
  }

  if (!unsupported.find(elem => elem === currentDriver)) {
    return null;
  }
  return (
    <small>
      Note: This feature is currently not supported for{' '}
      {driverToLabel[currentDriver]}
    </small>
  );
};
