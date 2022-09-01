import * as React from 'react';
import { SampleDBButton } from './SampleDBButton';

type Props = { onTrySampleDB: VoidFunction };

export const SampleDBSection: React.FC<Props> = ({ onTrySampleDB }) => {
  const [showSuccess, setShowSuccess] = React.useState(false);

  const onButtonClick = () => {
    onTrySampleDB();
    setShowSuccess(true);
  };

  return (
    <div className="flex flex-col w-full items-start">
      <div className="w-full flex justify-start items-center">
        <p className="m-0 mr-sm">
          Don&apos;t have a database? Try a read-only sample database:
        </p>
        <SampleDBButton onClick={onButtonClick} />
      </div>
      {showSuccess && (
        <i className="mt-xs text-green-800 text-sm">
          The database credentials have been populated in the form below. Please
          click &apos;Connect Database&apos; button to continue.
        </i>
      )}
    </div>
  );
};
