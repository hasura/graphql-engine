import React from 'react';
import { Driver, currentDriver } from '../../../dataSources';

const driverToLabel: Record<Driver, string> = {
  mysql: 'MySQL',
  postgres: 'PostgreSQL',
  mssql: 'MS Server',
  bigquery: 'Big Query',
};

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
