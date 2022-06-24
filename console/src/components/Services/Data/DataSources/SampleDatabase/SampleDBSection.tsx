import * as React from 'react';
import { SampleDBButton } from './SampleDBButton';

type Props = { onTrySampleDB: VoidFunction };

export const SampleDBSection: React.FC<Props> = ({ onTrySampleDB }) => {
  return (
    <div className="w-full flex justify-start items-center">
      <p className="m-0 mr-sm">Don&apos;t have a database?</p>
      <SampleDBButton onClick={onTrySampleDB} />
    </div>
  );
};
